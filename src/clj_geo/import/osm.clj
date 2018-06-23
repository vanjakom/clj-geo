(ns clj-geo.import.osm
  (:require
    [clj-common.logging :as logging]
    [clj-common.io :as io]
    [clj-common.http :as http]
    [clj-common.cache :as cache]
    [clj-geo.env :as env])
  (:import
    java.io.InputStream))

; model fns

(defn create-tile [zoom x y]
  {
    :zoom zoom
    :x x
    :y y})

(defn create-tile-data [zoom x y ^InputStream data]
  (assoc
    (create-tile zoom x y)
    :data
    data))


; math fns

; https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Lon..2Flat._to_tile_numbers_2
; n = 2 ^ zoom
; xtile = n * ((lon_deg + 180) / 360)
; ytile = n * (1 - (log(tan(lat_rad) + sec(lat_rad)) / π)) / 2
;
; n = 2 ^ zoom
; lon_deg = xtile / n * 360.0 - 180.0
; lat_rad = arctan(sinh(π * (1 - 2 * ytile / n)))
; lat_deg = lat_rad * 180.0 / π

(defn calclulate-tile [zoom location]
  ; zoom-shifted = 2 ^ zoom
  (let [zoom-shifted (bit-shift-left 1 zoom)
        lat-radians (Math/toRadians (:latitude location))
        xtile (int (Math/floor (* (/ (+ 180 (:longitude location)) 360) zoom-shifted)))
        ytile (int (Math/floor (* (/ (- 1
                                        (/
                                          (Math/log (+ (Math/tan lat-radians)
                                                       (/ 1 (Math/cos lat-radians))))
                                          Math/PI))
                                     2)
                                  zoom-shifted)))]
    (create-tile
      zoom
      (cond (< xtile 0) 0
            (>= xtile zoom-shifted) (- zoom-shifted 1)
            :else xtile)
      (cond (< ytile 0) 0
            (>= ytile zoom-shifted) (- zoom-shifted 1)
            :else ytile))))

(defn create-location->tile [zoom]
  (partial calclulate-tile zoom))

(defn tile->location [tile]
  ; zoom-shifted = 2 ^ zoom
  (let [zoom-shifted (bit-shift-left 1 (:zoom tile))
        longitude (- (* (/ (:x tile) zoom-shifted) 360) 180.0)
        latitude-rad (Math/atan (Math/sinh (* Math/PI (- 1 (* 2 (/ (:y tile) zoom-shifted))))))
        latitude (Math/toDegrees latitude-rad)]
    {:longitude longitude :latitude latitude}))

; https://en.wikipedia.org/wiki/Web_Mercator#Formulas
(defn calculate-x-y [zoom location]
  ; zoom-shifted = 2 ^ zoom
  (let [zoom-shifted (bit-shift-left 1 zoom)]
    [
      (*
        (/ 256 (* 2 Math/PI))
        zoom-shifted
        (+ (Math/toRadians (:longitude location)) Math/PI))
      (*
        (/ 256 (* 2 Math/PI))
        zoom-shifted
        (- Math/PI (Math/log
                     (Math/tan
                       (+
                         (/ Math/PI 4)
                         (/ (Math/toRadians (:latitude location)) 2))))))]))

(defn create-location->x-y [zoom]
  (partial calculate-x-y zoom))

(defn calclulate-tiles [zoom min-longitude max-longitude min-latitude max-latitude]
  (let [min-tile (calclulate-tile zoom {:longitude min-longitude :latitude min-latitude})
        max-tile (calclulate-tile zoom {:longitude max-longitude :latitude max-latitude})]
    (mapcat
      (fn [x]
        (map
          #(create-tile zoom x %)
          ; y is going from top left cornert
          (range (:y max-tile) (inc (:y min-tile)))))
      (range (:x min-tile) (inc (:x max-tile))))))


; retrieval fns

(defn create-toner-lines-url [{zoom :zoom x :x y :y}]
  (str "http://tile.stamen.com/toner-lines/" zoom "/" x "/" y ".png"))

(defn create-mapnik-from-osm-url [{zoom :zoom x :x y :y}]
  (str "http://tile.openstreetmap.org/" zoom "/" x "/" y ".png"))

(defn create-thunderforest-cycle-map-url [{zoom :zoom x :x y :y}]
  (if-let [apikey env/*thunderforest-apikey*]
    (str "http://tile.thunderforest.com/cycle/" zoom "/" x "/" y ".png?apikey=" apikey)
    (str "http://tile.thunderforest.com/cycle/" zoom "/" x "/" y ".png")))

(defn create-thunderforest-outdoors-map-url [{zoom :zoom x :x y :y}]
  (if-let [apikey env/*thunderforest-apikey*]
    (str "http://tile.thunderforest.com/outdoors/" zoom "/" x "/" y ".png?apikey=" apikey)
    (str "http://tile.thunderforest.com/outdoors/" zoom "/" x "/" y ".png")))


; visit https://wiki.openstreetmap.org/wiki/Tile_servers
(def tiles
  {
    :stamen-lines (var create-toner-lines-url)
    ; not working
    :mapnik-from-osm (var create-mapnik-from-osm-url)
    ; requires api key
    :thunderforest-cycle (var create-thunderforest-cycle-map-url)
    :thunderforest-outdoors (var create-thunderforest-outdoors-map-url)})


(defn cache-key-fn [url]
  (->
    ; remove params ( api key )
    (let [params-start (.indexOf url "?")]
      (if (> params-start 0) (.substring url 0 params-start) url))
    (.replace "http://" "")
    (.replace "/" "_")
    (.replace "." "_")))

(def ^:dynamic *tile-cache* (cache/create-local-fs-cache
                              {
                                :cache-path env/*tile-cache-path*
                                :key-fn cache-key-fn
                                :value-serialize-fn io/input-stream->bytes
                                :value-deserialize-fn io/bytes->input-stream}))

(defn retrieve-tile
  "Either retrives tile from cache or performs download"
  [url]
  (if-let [tile (*tile-cache* url)]
    (do
      (logging/report {:fn retrieve-tile :stage :retrieve-from-cache})
      tile)
    (if-let [tile (http/get-as-stream url)]
      (let [input-stream-cache-fn (io/cache-input-stream tile)]
        (logging/report {:fn retrieve-tile :stage :download-ok})
        (*tile-cache* url (input-stream-cache-fn))
        (input-stream-cache-fn))
      (do
        (logging/report {:fn retrieve-tile :stage :download-fail :url url})
        nil))))


(defn retrieve-tile-data-seq
  "Calculates tiles, downloads data if needed. Returns tile-data seq."
  [tile-name zoom min-longitude max-longitude min-latitude max-latitude]
  (let [tile-seq (calclulate-tiles
                   zoom
                   min-longitude max-longitude min-latitude max-latitude)]
    (doall
      (map
        #(create-tile-data
           (:zoom %) (:x %) (:y %) (retrieve-tile ((get tiles tile-name) %)))
        tile-seq))))
