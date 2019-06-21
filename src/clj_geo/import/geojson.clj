(ns clj-geo.import.geojson)

;;; all transformation functions work with deserialized JSON reporesentation

(defn location->feature [location]
  {
   :type "Feature"
   :properties
   {:tags (:tags location)}
   #_(reduce
      (fn [tags tag]
        (assoc tags tag "true"))
      {}
      (:tags location))
   :geometry  {
              :type "Point"
              :coordinates [(:longitude location) (:latitude location)]}})

(defn location-seq->geojson [location-seq]
  {
   :type "FeatureCollection"
   :features (map location->feature location-seq)})
