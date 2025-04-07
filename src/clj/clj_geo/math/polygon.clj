(ns clj-geo.math.polygon
  (:use clj-common.clojure))

(defn longitude [location]
  (:longitude location))

(defn latitude [location]
  (:latitude location))

(defn location [longitude latitude]
  {:longitude longitude :latitude latitude})

;; https://www.sanfoundry.com/java-program-check-whether-given-point-lies-given-polygon/
(defn location-inside
  "Is location inside polygon defined with location-seq. No need to close polygon
  ( first location also last )"
  [location-seq target]
  (let [on-segment (fn [p q r]
                     (and
                      (<= (longitude q) (max (longitude p) (longitude r)))
                      (>= (longitude q) (min (longitude p) (longitude r)))
                      (<= (latitude q) (max (latitude p) (latitude r)))
                      (>= (latitude q) (min (latitude p) (latitude r)))))
        orientation (fn [p q r]
                      (let [v (-
                               (* (- (latitude q) (latitude p))
                                  (- (longitude r) (longitude q)))
                               (* (- (longitude q) (longitude p))
                                  (- (latitude r) (latitude q))))]
                        (cond
                          (= v 0) 0
                          (> v 0) 1
                          :else 2)))
        intersect (fn [p1 q1 p2 q2]
                    (let [o1 (orientation p1 q1 p2)
                          o2 (orientation p1 q1 q2)
                          o3 (orientation p2 q2 p1)
                          o4 (orientation p2 q2 q1)]
                      (cond
                        (and (not (= o1 o2)) (not (= o3 o4)))
                        true

                        (and (= o1 0) (on-segment p1 p2 q1))
                        true

                        (and (= o2 0) (on-segment p1 q2 q1))
                        true

                        (and (= o3 0) (on-segment p2 p1 q2))
                        true

                        (and (= o4 0) (on-segment p2 q1 q2))
                        true

                        :else
                        false)))
        infinity 1000.0]
    (if (< (count location-seq) 3)
      false
      (let [extreme (location infinity (latitude target))
            [final count _] (reduce
                             (fn [[final count previous] next]
                               (if (intersect previous next target extreme)
                                 (if (= (orientation previous target next) 0)
                                   (reduced
                                    [(on-segment previous target next) nil nil])
                                   [nil (inc count) next])
                                 [nil count next]))
                             [nil 0 (first location-seq)]
                             (rest location-seq))]
        (cond
          (some? final) final
          (odd? count) true
          :else false)))))

#_(location-inside
 [
  (location 0 0) (location 10 0) (location 10 10) (location 0 10)]
 (location 20 20))

#_(location-inside
 [
  (location 0 0) (location 10 0) (location 10 10) (location 0 10)]
 (location 5 5))


#_(location-inside
 [
  (location 0 0) (location 10 0) (location 10 10) (location 0 10)]
 (location 8 1))


