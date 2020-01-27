(ns clj-geo.import.geojson)

;;; NOTE: obsolite, for trek-mate releated stuff use trek-mate.intergration.geojson

;;; all transformation functions work with deserialized JSON reporesentation

(defn location->feature [location]
  {
   :type "Feature"
   :properties
   (:tags location)
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

(defn way->feature
  "Way should be defined as map containing :tags map and :locations seq.
  Note: ways will be represented as LineString"
  [way]
  {
   :type "Feature"
   :properties (:tags way)
   :geometry {
              :type "LineString"
              :coordinates (map
                            (fn [location]
                              [(:longitude location) (:latitude location)])
                            (:locations way))}})

(defn way-seq->geojson [way-seq]
  {
   :type "FeatureCollection"
   :features (map way->feature way-seq)})
