(ns clj-geo.import.geojson
  (:require
   [clojure.core.async :as async]
   [clj-common.context :as context]
   [clj-common.io :as io]
   [clj-common.json :as json]
   [clj-common.localfs :as fs]))

(defn geojson [feature-seq]
  {
   :type "FeatureCollection"
   :features feature-seq})

(def ^:dynamic *style-stroke-color* "#0000FF") ;; blue
(def ^:dynamic *style-stroke-width* 2)

(defn point [longitude latitude properties]
  {
   :type "Feature"
   :properties properties
   :geometry  {
               :type "Point"
               :coordinates [longitude latitude]}})

(defn marker [longitude latitude body]
  (point
   longitude
   latitude
   {
    :marker-body body}))

(defn line-string
  ([location-seq]
   (line-string {} location-seq))
  ([properties location-seq]
   {
    :type "Feature"
    :properties (assoc
                 properties
                 "stroke" *style-stroke-color*
                 "stroke-width" *style-stroke-width*)
    :geometry {
               :type "LineString"
               :coordinates (map
                             (fn [location]
                               [(:longitude location) (:latitude location)])
                             location-seq)}}))

(defn multi-line-string
  "Input should be sequence of coordinate sequences"
  ([location-seq-seq]
   (multi-line-string {} location-seq-seq))
  ([properties location-seq-seq]
   {
    :type "Feature"
    :properties (assoc
                 properties
                 "stroke" *style-stroke-color*
                 "stroke-width" *style-stroke-width*)
    :geometry {
               :type "MultiLineString"
               :coordinates (map
                             (fn [location-seq]
                               (map
                                (fn [location]
                                  [(:longitude location) (:latitude location)])
                                location-seq))
                             location-seq-seq)}}))

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

(defn way->line-string
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

;; old way, creating geojson from single feature

(defn location-seq->geojson [location-seq]
  {
   :type "FeatureCollection"
   :features (map location->feature location-seq)})

(defn track->geojson
  "Assuming track format as in JSON backup files.
  Keeping tags in root object to allow multiple line strings for track once tiles
  are introduced"
  [track]
  {
   :type "FeatureCollection"
   :properties (dissoc track :locations)
   :features [
              {
               :type "Feature"
               :properties {}
               :geometry {
                          :type "LineString"
                          :coordinates (map
                                        (fn [location]
                                          [(:longitude location) (:latitude location)])
                                        (:locations track))}}]})

(defn way-seq->geojson [way-seq]
  {
   :type "FeatureCollection"
   :features (map way->line-string way-seq)})

(defn geojson->location-seq [input-stream]
  (let [geojson (json/read-keyworded input-stream)]
    (mapcat
     (fn [feature]
       (map
        (fn [location]
          {
           :longitude (first location)
           :latitude (second location)})
        (:coordinates (:geometry feature))))
     (:features geojson))))

(defn write-geojson-go
  [context path feature-in]
  (async/go
    (context/set-state context "init")
    (with-open [os (fs/output-stream path)]
      (io/write-line os "{")
      (io/write-line os "\t\"type\":\"FeatureCollection\",")
      (io/write-line os "\t\"features\": [")

      (loop [feature (async/<! feature-in)
             first true]
        (context/set-state context "step")
        (when feature
          (context/increment-counter context "feature-out")
          (when (not first)
            (io/write-string os ",\n"))
          (let [serialized (json/write-to-string feature)]
            (io/write-string os (str "\t\t" serialized)))
          (recur
           (async/<! feature-in)
           false)))

      (io/write-line os "\t]")
      (io/write-line os "}")
      (context/set-state context "completion"))))


