(ns clj-geo.math.tile)

;;; tile coordinate system ( web mercator )
;;; 0,0 is upper left corner

(def tile
  {
   ;; lower left coordinate system
   :zoom :long
   :x :long
   :y :long})

(def tile-data
  (assoc
   tile
   ;; initial implementation uses InputStream, this needs to be fixed before switch to CLJC
   :data :bytes))

(def tile-bounds
  [
   [:zoom :long]
   [:min-x :long]
   [:max-x :long]
   [:min-y :long]
   [:max-y :long]])


;;; v1, before fixing latitude -90.0
#_(defn zoom->location->tile [zoom]
  (fn [location]
    ;; zoom-shifted = 2 ^ zoom
    (let [zoom-shifted (bit-shift-left 1 zoom)
          lat-radians (Math/toRadians (:latitude location))
          xtile (int (Math/floor (*
                                  (/
                                   (+ 180 (:longitude location))
                                   360)
                                  zoom-shifted)))
          ytile (int (Math/floor (*
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
                                  zoom-shifted)))]
      {
       :zoom zoom
       :x (cond (< xtile 0) 0
                (>= xtile zoom-shifted) (- zoom-shifted 1)
                :else xtile)
       :y (cond (< ytile 0) 0
                (>= ytile zoom-shifted) (- zoom-shifted 1)
                :else ytile)})))

(defn zoom->location->tile [zoom]
  (fn [location]
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
      {
       :zoom zoom
       :x (cond (< xtile 0) 0
                (>= xtile zoom-shifted) (- zoom-shifted 1)
                :else (int xtile))
       :y (cond (< ytile 0) 0
                (>= ytile zoom-shifted) (- zoom-shifted 1)
                :else (int ytile))})))

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
