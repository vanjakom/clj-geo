(ns clj-geo.utils)

(defn bounding-box
  "Returns min longitude, latitude and max longitude latitude"
  [locations]
  (if (not (empty? locations))
    (reduce
      (fn [[min-longitude max-longitude min-latitude max-latitude] location]
        [
          (min min-longitude (:longitude location))
          (max max-longitude (:longitude location))
          (min min-latitude (:latitude location))
          (max max-latitude (:latitude location))])
      [
        (:longitude (first locations))
        (:longitude (first locations))
        (:latitude (first locations))
        (:latitude (first locations))]
      locations)))
