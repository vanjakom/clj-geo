(ns clj-geo.import.geojson)

;;; NOTE: obsolite, for trek-mate releated stuff use trek-mate.intergration.geojson

;;; all transformation functions work with deserialized JSON reporesentation

(defn location->feature [location]
  {
   :type "Feature"
   :properties (dissoc location :longitude :latitude)
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

;; new approach

(defn location->point [location]
  {
   :type "Feature"
   :properties (dissoc location :longitude :latitude)
   :geometry  {
              :type "Point"
              :coordinates [(:longitude location) (:latitude location)]}})

(defn location-seq->line-string
  [location-seq]
  {
   :type "Feature"
   :properties {}
   :geometry {
              :type "LineString"
              :coordinates (map
                            (fn [location]
                              [(:longitude location) (:latitude location)])
                            location-seq)}})

(defn location-seq-seq->multi-line-string
  [location-seq-seq]
  {
   :type "Feature"
   :properties {}
   :geometry {
              :type "MultiLineString"
              :coordinates (map
                            (fn [location-seq]
                              (map
                               (fn [location]
                                 [(:longitude location) (:latitude location)])
                               location-seq))
                            location-seq-seq)}})

(defn geojson [feature-seq]
  {
   :type "FeatureCollection"
   :features feature-seq})
