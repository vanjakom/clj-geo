(ns clj-geo.math.tile
  (:use clj-common.clojure))

;;; tile coordinate system ( web mercator )
;;; 0,0 is upper left corner
;;; zoom 0 is lower zoom than zoom 5
;;; zoom 5 is higher zoom than zoom 0

#_(model
   tile
   [[:zoom :long] [:x :long] [:y :long]])

#_(model tile-bounds
  [
   [:zoom :long]
   [:min-x :long]
   [:max-x :long]
   [:min-y :long]
   [:max-y :long]])

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

(defn zoom->location->point [zoom]
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

(defn tile->location [tile]
  ; zoom-shifted = 2 ^ zoom
  (let [zoom-shifted (bit-shift-left 1 (:zoom tile))
        longitude (- (* (/ (:x tile) zoom-shifted) 360) 180.0)
        latitude-rad (Math/atan (Math/sinh (* Math/PI (- 1 (* 2 (/ (:y tile) zoom-shifted))))))
        latitude (Math/toDegrees latitude-rad)]
    {:longitude longitude :latitude latitude}))

(defn tile->location-bounds [tile]
  (let [upper-left-location (tile->location tile)
        lower-right-location (tile->location (assoc
                                              tile
                                              :x (inc (:x tile))
                                              :y (inc (:y tile))))]
    [
     (:longitude upper-left-location)
     (:longitude lower-right-location)
     (:latitude lower-right-location)
     (:latitude upper-left-location)]))

(defn calculate-tile-bounds-from-tile-seq
  [tile-seq]
  (transduce
   identity
   (fn
     ([] [nil nil nil nil nil])
     ([[zoom min-x max-x min-y max-y] tile]
      [
       (or zoom (:zoom tile))
       (min (or min-x (:x tile)) (:x tile))
       (max (or max-x (:x tile)) (:x tile))
       (min (or min-y (:y tile)) (:y tile))
       (max (or max-y (:y tile)) (:y tile))])
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

;;; having problem with higher level zoom, replaced with more specifc
;;; tile->zoom-->point->tile-offset
#_(defn zoom->zoom->point->tile-offset
  "Convert tile points from upper zoom level to lower one.
  Note: doesn't work when zoom-out  > zoom-in and point does
  not belong to tile ..."
  [zoom-in zoom-out [x y]]
  (let [zoom-diff (- zoom-in zoom-out)
        divider (Math/pow 2 zoom-diff)]
    [(rem (int (/ x divider)) 256) (rem (int (/ y divider)) 256)]))

#_(zoom->zoom->point->tile-offset 16 15 [200 200]) ; [100 100]
#_(zoom->zoom->point->tile-offset 16 16 [200 200]) ; [200 200]
#_(zoom->zoom->point->tile-offset 16 17 [200 200]) ; [144 144]

#_(zoom->zoom->point->tile-offset 16 15 [100 100]) ; [50 50]
#_(zoom->zoom->point->tile-offset 16 16 [100 100]) ; [100 100]
#_(zoom->zoom->point->tile-offset 16 17 [100 100]) ; [200 200]

(defn zoom->zoom->point->tile
  "Works only when zoom-in > zoom-out."
  [zoom-in zoom-out [x y]]
  (let [divider (* 256 (Math/pow 2 (- zoom-in zoom-out)))]
    [
     zoom-out
     (int (Math/floor (/ x divider)))
     (int (Math/floor (/ y divider)))]))

(defn zoom->point->tile
  [zoom]
  (fn [point]
    ;; todo
    ))

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

;;; depricated, has problems with over zoom, rendering zoom 16 location
;;; on zoom 17, use tile->zoom-->point->tile-offset
#_(defn tile->zoom-->point->bounds?
  "Tests if point on specified zoom level belongs to given tile.
  Zoom of tile is lower level than given zoom. Used to test if given set of
  points belongs to tile"
  [tile zoom]
  (let [tiles (zoom->tile->tile-seq zoom tile)
        divider (Math/pow 2 (- zoom (first tile)))
        min-x (* 256 (reduce min (get (first tiles) 1) (map second tiles)))
        max-x (+ min-x (* 256 divider))
        min-y (* 256 (reduce min (get (first tiles) 2) (map (partial-right get 2) tiles)))
        max-y (+ min-y (* 256 divider))]
    (println divider min-x max-x min-y max-y)
    (fn [[x y]]
      (and
       (>= x min-x) (<= x max-x) (>= y min-y) (<= y max-y)))))

#_((tile->zoom-->point->bounds? [17 0 0] 16) [100 100] ) ; true
#_((tile->zoom-->point->bounds? [17 0 0] 16) [200 200] ) ; false
#_((tile->zoom-->point->bounds? [17 1 1] 16) [200 200] ) ; false <- problem ...
#_((tile->zoom-->point->bounds? [16 0 0] 16) [100 100] ) ; true
#_((tile->zoom-->point->bounds? [16 0 0] 16) [200 200] ) ; true
#_((tile->zoom-->point->bounds? [15 0 0] 16) [100 100] ) ; true
#_((tile->zoom-->point->bounds? [15 0 0] 16) [200 100] ) ; true

#_(zoom->tile->tile-seq 16 [17 0 0]) ; [[16 0 0]] 
#_(zoom->tile->tile-seq 16 [17 1 1]) ; [[16 0 0]]
#_(zoom->tile->tile-seq 16 [17 2 2]) ; [[16 1 1]]
#_(zoom->tile->tile-seq 16 [17 3 3]) ; [[16 1 1]]

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


