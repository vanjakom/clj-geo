(ns clj-geo.import.osm
  (:use
   clj-common.clojure)
  (:require
   [clojure.data.xml :as xml]
   [clj-common.as :as as]
   [clj-common.logging :as logging]
   [clj-common.io :as io]
   [clj-common.http :as http]
   [clj-common.cache :as cache]
   [clj-common.view :as view]
   [clj-common.context :as context]
   [clj-geo.env :as env])
  (:import
   java.io.InputStream))

(declare parse-node)
(declare parse-tag)
(declare parse-way)
(declare parse-node-ref)

(defn parse-entry [entry]
  (cond
    (= (:tag entry) :node) (parse-node entry)
    (= (:tag entry) :tag) (parse-tag entry)
    (= (:tag entry) :way) (parse-way entry)
    (= (:tag entry) :nd) (parse-node-ref entry)
    :default nil))

(defn parse-node [entry]
  (let [longitude (as/as-double (:lon (:attrs entry)))
        latitude (as/as-double (:lat (:attrs entry)))
        id (as/as-long (:id (:attrs entry)))
        content (map parse-entry (:content entry))
        tags (reduce
              (fn [tags {key :key value :value}]
                (assoc
                 tags
                 key
                 value))
              {}
              (filter #(= (:type %) :tag) content))]
    {
     :type :node
     :id id
     :longitude longitude
     :latitude latitude
     :tags tags}))

(defn parse-way [entry]
  (let [id (as/as-long (:id (:attrs entry)))
        content (map parse-entry (:content entry))
        nodes (filter #(= (:type %) :node-ref) content)
        tags (reduce
              (fn [tags {key :key value :value}]
                (assoc
                 tags
                 key
                 value))
              {}
              (filter #(= (:type %) :tag) content))]
    {
     :type :way
     :id id
     :nodes nodes
     :tags tags}))

;;; initially tried with tags having keyword for name but it seems there is no standard
;; for tag names, returning to string, string pairs ...
(defn parse-tag [entry]
  {
   :type :tag
   :key (:k (:attrs entry))
   :value (:v (:attrs entry))})

(defn parse-node-ref [entry]
  (let [ref (as/as-long (:ref (:attrs entry)))]
    {
     :type :node-ref
     :id ref}))

(defn read-osm
  "Reads fully in memory OSM file."
  [input-stream]
  (todo-warn "support relations")
  (let [content (:content (xml/parse input-stream))
        [bounds nodes ways] (reduce
                             (fn [[bounds nodes ways] entry]
                               (let [element (parse-entry entry)]
                                 (cond
                                   (= (:type element) :node) [bounds (conj nodes element) ways]
                                   (= (:type element) :way) [bounds nodes (conj ways element)]
                                   :default [bounds nodes ways])))
                             [nil '() '()]
                             content)]
    {
     :nodes nodes
     :ways ways}))

(defn stream-osm
  "Streams parsed osm data. To be used for fast filtering."
  [input-stream]
  (map
   parse-entry
   (:content (xml/parse input-stream))))

(defn merge-nodes-into-way
  ([node-seq way-seq]
   (let [node-map (view/seq->map :id node-seq)]
     (map
      (fn [way]
        (assoc
         way
         :nodes
         (map
          #(get node-map (:id %))
          (:nodes way))))
      way-seq)))
  ([osm]
   (merge-nodes-into-way (:nodes osm) (:ways osm))))
