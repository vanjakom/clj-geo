(ns clj-geo.import.tile
  (:require
   [clj-common.logging :as logging]
   [clj-common.io :as io]
   [clj-common.http :as http]
   [clj-common.cache :as cache]
   [clj-geo.math.tile :as math]
   [clj-geo.env :as env]))

(defn create-stamen-url [{zoom :zoom x :x y :y}]
  (str "http://tile.stamen.com/toner/" zoom "/" x "/" y ".png"))

(defn create-stamen-lines-url [{zoom :zoom x :x y :y}]
  (str "http://tile.stamen.com/toner-lines/" zoom "/" x "/" y ".png"))

(defn create-stamen-watercolor-url [{zoom :zoom x :x y :y}]
  (str "http://tile.stamen.com/watercolor/" zoom "/" x "/" y ".png"))

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

(defn create-osm-url [{zoom :zoom x :x y :y}]
  (str
   "https://tile.openstreetmap.org/" zoom "/" x "/" y ".png"))

(defn create-mapbox-raster-url [username style access-token]
  (fn [{zoom :zoom x :x y :y}]
    (str
     "https://api.mapbox.com/styles/v1/"
     username
     "/"
     style
     "/tiles/256/{z}/{x}/{y}"
     "?access_token="
     access-token)))


;;; visit https://wiki.openstreetmap.org/wiki/Tile_servers
(def tiles
  {
   :stamen (var create-stamen-url)
   :stamen-lines (var create-stamen-lines-url)
   :stamen-watercolor (var create-stamen-watercolor-url)
   ;; not working
   :mapnik-from-osm (var create-mapnik-from-osm-url)
   ;; requires api key
   :thunderforest-cycle (var create-thunderforest-cycle-map-url)
   :thunderforest-outdoors (var create-thunderforest-outdoors-map-url)})


(defn cache-key-fn [url]
  (->
   ;; remove params ( api key )
   (let [params-start (.indexOf url "?")]
     (if (> params-start 0) (.substring url 0 params-start) url))
   (.replace "http://" "")
   (.replace "https://" "")
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
        (logging/report {:fn retrieve-tile :stage :download-ok :url url})
        (*tile-cache* url (input-stream-cache-fn))
        (input-stream-cache-fn))
      (do
        (logging/report {:fn retrieve-tile :stage :download-fail :url url})
        nil))))

(defn retrieve-tile-data-seq
  "Calculates tiles, downloads data if needed. Returns tile-data seq."
  [tile-name zoom min-longitude max-longitude min-latitude max-latitude]
  (let [tile-seq (math/calculate-tile-seq
                  zoom
                  min-longitude max-longitude min-latitude max-latitude)]
    (doall
     (map
      #(assoc
        %
        :data
        (retrieve-tile ((get tiles tile-name) %)))
      tile-seq))))
