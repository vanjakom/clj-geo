(ns clj-geo.math.tile
  (:use clj-common.clojure))

;;; tile coordinate system ( web mercator )
;;; 0,0 is upper left corner
;;; zoom 0 is lower zoom than zoom 5
;;; zoom 5 is higher zoom than zoom 0

;;; tile - [zoom x y]
;;; point [x y]
;;; tile-offset [offset-x offset-y]
;;; tile-bounds [zoom min-x max-x min-y max-y]


;;; https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Clojure
(defn zoom->location->tile [zoom location]
  ;; zoom-shifted = 2 ^ zoom
  (let [zoom-shifted (bit-shift-left 1 zoom)
        lat-radians (Math/toRadians (:latitude location))
        xtile (Math/floor (*
                           (/
                            (+ 180 (:longitude location))
                            360)
                           zoom-shifted))
        ytile (Math/floor (*
                           (/
                            (-
                             1
                             (/
                              (Math/log
                               (+
                                (Math/tan lat-radians)
                                (/ 1 (Math/cos lat-radians))))
                              Math/PI))
                            2)
                           zoom-shifted))]
    [zoom
     (cond (< xtile 0) 0
              (>= xtile zoom-shifted) (- zoom-shifted 1)
              :else (long xtile))
     (cond (< ytile 0) 0
              (>= ytile zoom-shifted) (- zoom-shifted 1)
              :else (long ytile))]))

;;; https://en.wikipedia.org/wiki/Web_Mercator_projection#Formulas
(defn zoom-->location->point [zoom]
  ;; zoom-shifted = 2 ^ zoom
  (fn [location]
    (try
      (let [zoom-shifted (bit-shift-left 1 zoom)]
        [
         (int
          (*
           (/ 256 (* 2 Math/PI))
           zoom-shifted
           (+ (Math/toRadians (:longitude location)) Math/PI)))
         (if (> (:latitude location) -90.0)
           (int
            (*
             (/ 256 (* 2 Math/PI))
             zoom-shifted
             (- Math/PI (Math/log
                         (Math/tan
                          (+
                           (/ Math/PI 4)
                           (/ (Math/toRadians (:latitude location)) 2)))))))
           255)])
      (catch Exception e (throw
                          (ex-info
                           "Exception with data"
                           {
                            :zoom zoom
                            :location location}
                           e))))))
(def zoom->location->point zoom-->location->point)

;;; https://en.wikipedia.org/wiki/Web_Mercator_projection#Formulas
(defn zoom->point->location [zoom [x y]]
  [
   (Math/toDegrees
    (-
     (/
      (* x 2 Math/PI)
      (* 256 (Math/pow 2 zoom)))
     Math/PI))
   (Math/toDegrees
    (*
     2
     (-
      (Math/atan
       (Math/pow
        Math/E
        (-
         Math/PI
         (/
          (* y 2 Math/PI)
          (* 256 (Math/pow 2 zoom))))))
      (/ Math/PI 4))))])

;;; https://gist.githubusercontent.com/maptiler/fddb5ce33ba995d5523de9afdf8ef118/raw/d7565390d2480bfed3c439df5826f1d9e4b41761/globalmaptiles.py
(defn longitude->latitude->point-in-meters [longitude latitude]
  (let [origin-shift 20037508.342789244]
    [
     (* longitude (/ origin-shift 180))
     (/
      (Math/log (Math/tan (/ (* (+ 90 latitude) Math/PI) 360.0)))
      (/ Math/PI 180.0))]))

(defn tile->location [[zoom x y]]
  ; zoom-shifted = 2 ^ zoom
  (let [zoom-shifted (bit-shift-left 1 zoom)
        longitude (- (* (/ x zoom-shifted) 360) 180.0)
        latitude-rad (Math/atan (Math/sinh (* Math/PI (- 1 (* 2 (/ y zoom-shifted))))))
        latitude (Math/toDegrees latitude-rad)]
    {:longitude longitude :latitude latitude}))

(defn tile->location-bounds [[zoom x y :as tile]]
  (let [upper-left-location (tile->location tile)
        lower-right-location (tile->location [zoom (inc x) (inc y)])]
    [
     (:longitude upper-left-location)
     (:longitude lower-right-location)
     (:latitude lower-right-location)
     (:latitude upper-left-location)]))

(defn tile-seq->tile-bounds
  [tile-seq]
  (transduce
   identity
   (fn
     ([] [nil nil nil nil])
     ([[min-x max-x min-y max-y] [_ x y]]
      [
       (min (or min-x x) x)
       (max (or max-x x) x)
       (min (or min-y y) y)
       (max (or max-y y) y)])
     ([state] state))
   tile-seq))

(defn calculate-tile-seq
  ([zoom location-bounds]
   (calculate-tile-seq
    zoom
    (nth location-bounds 0)
    (nth location-bounds 1)
    (nth location-bounds 2)
    (nth location-bounds 3)))
  ([zoom min-longitude max-longitude min-latitude max-latitude]
   (let [min-tile ((zoom->location->tile zoom) {:longitude min-longitude :latitude min-latitude})
        max-tile ((zoom->location->tile zoom) {:longitude max-longitude :latitude max-latitude})]
    (mapcat
      (fn [x]
        (map
         (fn [y]
           {:zoom zoom :x x :y y})
          ; y is going from top left cornert
          (range (:y max-tile) (inc (:y min-tile)))))
      (range (:x min-tile) (inc (:x max-tile)))))))

(defn calculate-tile-bounds-from-location-bounds
  [zoom min-longitude max-longitude min-latitude max-latitude]
  (let [min-tile ((zoom->location->tile zoom) {:longitude min-longitude :latitude min-latitude})
        max-tile ((zoom->location->tile zoom) {:longitude max-longitude :latitude max-latitude})]
    [zoom (:x min-tile) (:x max-tile) (:y min-tile) (:y max-tile)]))

(defn calculate-location-bounds-from-tile-seq
  [tile-seq]
  (reduce
   (fn [[state-min-lon state-max-lon state-min-lat state-max-lat]
        [min-lon max-lon min-lat max-lat]]
     [(min (or state-min-lon min-lon) min-lon)
      (max (or state-max-lon max-lon) max-lon)
      (min (or state-min-lat min-lat) min-lat)
      (max (or state-max-lat max-lat) max-lat)])
   [nil nil nil nil]
   (map tile->location-bounds tile-seq)))

(defn zoom->zoom->point->tile
  "Works only when zoom-in > zoom-out."
  [zoom-in zoom-out [x y]]
  (let [divider (* 256 (Math/pow 2 (- zoom-in zoom-out)))]
    [
     zoom-out
     (int (Math/floor (/ x divider)))
     (int (Math/floor (/ y divider)))]))

(defn zoom->tile->tile-seq
  "Calculates tile(s) which represent given tile on different zoom level. If
  zoom level is lower only single tile will be returned, if it's higher multiple
  tiles will be returned"
  [zoom-out [zoom-in x y]]
  (cond
    (< zoom-out zoom-in)
    (let [divider (Math/pow 2 (- zoom-in zoom-out))]
      [[
        zoom-out
        (int (Math/floor (/ x divider)))
        (int (Math/floor (/ y divider)))]])
    (> zoom-out zoom-in)
    (let [multiplicator (int (Math/pow 2 (- zoom-out zoom-in)))]
      (mapcat
       (fn [x]
         (map
          (fn [y] [zoom-out x y])
          (range (* y multiplicator) (* (inc y) multiplicator))))
       (range (* x multiplicator) (* (inc x) multiplicator))))
    :else [[zoom-in x y]]))

(defn location->tile-seq
  "Returns tiles from 0 up 18 zoom level for location. Used for debugging"
  [location]
  (map
   (fn [zoom] (zoom->location->tile zoom location))
   (range 0 19)))

(defn zoom->zoom-->point->point
  "Transforms point from one zoom to another"
  [zoom-in zoom-out]
  (let [multiplicator (Math/pow 2 (- zoom-out zoom-in))]
    (fn [[x y]]
      [(* x multiplicator) (* y multiplicator)])))

(defn zoom->tile-->point->tile-offset
  "Prepares conversion function for rendering of given tile, by transforming
  points of given zoom"
  [zoom-in [zoom-out x-tile-out y-tile-out]]
  (let [point-convert-fn (zoom->zoom-->point->point zoom-in zoom-out)]
    (fn [point-in]
      (let [[x-out y-out] (point-convert-fn point-in)
            x-offset-zero (* x-tile-out 256)
            y-offset-zero (* y-tile-out 256)
            x-offset-out (- x-out x-offset-zero)
            y-offset-out (- y-out y-offset-zero)]
        (if (and
             (> x-offset-out 0) (< x-offset-out 256)
             (> y-offset-out 0) (< y-offset-out 256))
          [x-offset-out y-offset-out]
          nil)))))

(defn zoom->tile-bounds->zoom->point->in?
  "Tests if point on given zoom level belongs to tile
  Note: similar fn: zoom->tile-->point->tile-offset"
  [zoom-tile [min-x max-x min-y max-y] zoom-point point]
  (let [[x y] ((zoom->zoom-->point->point zoom-point zoom-tile) point)]
    (and
     (> x (* 256 min-x))
     (< x (* 256 max-x))
     (> y (* 256 min-y))
     (< y (* 256 max-y)))))
