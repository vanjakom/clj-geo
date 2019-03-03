(ns clj-geo.math.core
  (:use clj-common.test)
  (:use clj-common.clojure))

(def model
  {
    :location
    {
      :longitude :double
      :latitude :double}

    :rect
    {
      :min-longitude :double
      :min-latitude :double
      :max-longitude :double
      :max-latitude :double}
    :bounding-box
    [
      :min-longitude
      :max-longitude
      :min-latitude
      :max-latitude]})

(defn create-location [longitude latitude]
  {:longitude longitude :latitude latitude})

;;; http://stackoverflow.com/questions/3694380/calculating-distance-between-two-points-using-latitude-longitude-what-am-i-doi
(defn distance [location1 location2]
  (let [lat1 (:latitude location1) lon1 (:longitude location1)
        lat2 (:latitude location2) lon2 (:longitude location2)
        earth-radius 6371]
    (let [lat-distance (Math/toRadians (- lat2 lat1))
          lon-distance (Math/toRadians (- lon2 lon1))
          sin-lat-distance-half (Math/sin (/ lat-distance 2))
          sin-lon-distance-half (Math/sin (/ lon-distance 2))
          a (+
              (* sin-lat-distance-half sin-lat-distance-half)
              (*
                (Math/cos (Math/toRadians lat1))
                (Math/cos (Math/toRadians lat2))
                sin-lon-distance-half
                sin-lon-distance-half))
          c (*
              (Math/atan2
                (Math/sqrt a)
                (Math/sqrt (- 1 a)))
              2)]
      (*
        earth-radius
        c
        1000))))

(defn distance->human-string [distance]
  (cond
    (> distance 1000) (str (long (/ distance 1000)) "km")
    :else (str (int distance) "m")))

;;; depricated
(def distance-two-locations distance)

(defn speed-km [location1 location2]
  (let [distance (distance location1 location2)
        duration (- (:timestamp location2) (:timestamp location1))]
    (if (and (> distance 0) (> duration 0))
      (* 3.6 (/ distance duration))
      0)))

(defn angle-to-radians [angle] (Math/toRadians angle))

(defn radians-to-degrees [radians] (Math/toDegrees radians))

;;; Calculating Bearing or Heading angle between two points
;;; http://www.igismap.com/formula-to-find-bearing-or-heading-angle-between-two-points-latitude-longitude/
(defn bearing [location1 location2]
  (let [latitude1 (angle-to-radians (:latitude location1))
        longitude1 (angle-to-radians (:longitude location1))
        latitude2 (angle-to-radians (:latitude location2))
        longitude2 (angle-to-radians (:longitude location2))
        delta-longitude (angle-to-radians (- (:longitude location2) (:longitude location1)))
        x (* (Math/cos latitude2) (Math/sin delta-longitude))
        y (-
            (* (Math/cos latitude1) (Math/sin latitude2))
            (* (Math/sin latitude1) (Math/cos latitude2) (Math/cos delta-longitude)))]
    (radians-to-degrees (Math/atan2 x y))))

;;; depricated use location-in-bouding-box
(defn location-in-rect
  "location :location
   rect :rect"
  [location rect]
  (let [longitude (:longitude location)
        latitude (:latitude location)]
    (and
      (> longitude (:min-longitude rect))
      (< longitude (:max-longitude rect))
      (> latitude (:min-latitude rect))
      (< latitude (:max-latitude rect)))))

;;; common functions for multiple-reduce
(defn min-aggregate [field state location] (min state (get location field)))

(defn max-aggregate [field state location] (max state (get location field)))

(def max-aggregate-longitude (partial max-aggregate :longitude))
(def min-aggregate-longitude (partial min-aggregate :longitude))
(def max-aggregate-latitude (partial max-aggregate :latitude))
(def min-aggregate-latitude (partial min-aggregate :latitude))

(defn rect-for-location-seq [location-seq]
  (let [first-location (first location-seq)
        aggregate (multiple-reduce
                    [:max-longitude (:longitude first-location) max-aggregate-longitude
                     :min-longitude (:longitude first-location) min-aggregate-longitude
                     :max-latitude (:latitude first-location) max-aggregate-latitude
                     :min-latitude (:latitude first-location) min-aggregate-latitude]
                    location-seq)
        longitude-span (- (:max-longitude aggregate) (:min-longitude aggregate))
        center-longitude (+ (:min-longitude aggregate) (/ longitude-span 2))
        latitude-span (- (:max-latitude aggregate) (:min-latitude aggregate))
        center-latitude (+ (:min-latitude aggregate) (/ latitude-span 2))]
    [center-longitude center-latitude longitude-span latitude-span]))

(defn location-seq->longitude-latitude-radius [location-seq]
  (let [first-location (first location-seq)
        aggregate (multiple-reduce
                    [:max-longitude (:longitude first-location) max-aggregate-longitude
                     :min-longitude (:longitude first-location) min-aggregate-longitude
                     :max-latitude (:latitude first-location) max-aggregate-latitude
                     :min-latitude (:latitude first-location) min-aggregate-latitude]
                    location-seq)]
    [
      (+
        (:min-longitude aggregate)
        (/ (- (:max-longitude aggregate) (:min-longitude aggregate)) 2))
      (+
        (:min-latitude aggregate)
        (/ (- (:max-latitude aggregate) (:min-latitude aggregate)) 2))
      (distance
        {:longitude (:min-longitude aggregate) :latitude (:min-latitude aggregate)}
        {:longitude (:max-longitude aggregate) :latitude (:max-latitude aggregate)})]))

(defn bounding-box-reducing-fn
  ([] [nil nil nil nil])
  ([[min-longitude max-longitude min-latitude max-latitude] location]
   [
    (min (or min-longitude (:longitude location)) (:longitude location))
    (max (or max-longitude (:longitude location)) (:longitude location))
    (min (or min-latitude (:latitude location)) (:latitude location))
    (max (or max-latitude (:latitude location)) (:latitude location))])
  ([state] state))

(defn bounding-box [location-seq]
  "Returns [min-longitude max-longitude min-latitude max-latitude]"
  (reduce
    (fn [[min-longitude max-longitude min-latitude max-latitude]
         {longitude :longitude latitude :latitude}]
      [(min longitude (or min-longitude longitude))
       (max longitude (or max-longitude longitude))
       (min latitude (or min-latitude latitude))
       (max latitude (or max-latitude latitude))])
    [nil nil nil nil]
    location-seq))

(test
  "bounding-box test"
  (= (bounding-box
       (list
         {:longitude 20 :latitude 40}
         {:longitude 21 :latitude 39}))
     [20 21 39 40]))

(defn bounding-box->longitude-latitude-radius
  "Converts bounding box to longitude, latitude, radius triple"
  [[min-longitude max-longitude min-latitude max-latitude]]
  [
      (+
        min-longitude
        (/ (- max-longitude min-longitude) 2))
      (+
        min-latitude
        (/ (- max-latitude min-latitude) 2))
      (distance
        {:longitude min-longitude :latitude min-latitude}
        {:longitude max-longitude :latitude max-latitude})])

; https://gis.stackexchange.com/questions/5821/calculating-latitude-longitude-x-miles-from-point
(defn longitude-latitude-radius->bounding-box
  "Converts longitude and latitude of location with given radius to bounding box.
  Takes given latitude for longitude distance calculations, not precise on large distances.
  Radius in meters."
  [longitude latitude radius]
  (let [latitude-distance (/ (double radius) 111111)
        longitude-distance (/ latitude-distance (Math/cos (Math/toRadians latitude)))]
    [
     (- longitude longitude-distance)
     (+ longitude longitude-distance)
     (- latitude latitude-distance)
     (+ latitude latitude-distance)]))

(defn location-in-bounding-box?
  "Checks if location belongs to bounding box
  Bounding box is defined with [min-longitude max-longitude min-latitude max-latitude]"
  [bounding-box location]
  (try
    (let [longitude (:longitude location)
          latitude (:latitude location)]
      (and
       (> longitude (get bounding-box 0))
       (< longitude (get bounding-box 1))
       (> latitude (get bounding-box 2))
       (< latitude (get bounding-box 3))))
    (catch Exception ex (throw
                         (ex-info
                          "Exception in location-in-bounding-box"
                          {:bounding-box bounding-box :location location}
                          ex)))))

