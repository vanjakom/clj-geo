(ns clj-geo.import.okapi)

(require '[clojure.core.reducers :as r])

(require '[clj-common.path :as path])
(require '[clj-common.localfs :as fs])
(require '[clj-common.json :as json])
(require '[clj-common.logging :as logging])
(require '[clj-common.exception :as exception])

(defn full-dump-metadata [path]
  (with-open [is (fs/input-stream (path/child path "index.json"))]
    (assoc
      (json/read-keyworded is)
      :path
      path)))

(def format-object-transform
  (map
    (fn [raw]
      (exception/with-data raw
        (with-meta
          (let [data (:data raw)
                [latitude longitude] (.split (:location data) "\\|")]
            (assoc
              (dissoc data :location)
              :longitude (Double/parseDouble longitude)
              :latitude (Double/parseDouble latitude)))
          { ::type (keyword (:object_type raw))})))))

(defn geocache? [object]
  (= (::type (meta object)) :geocache))

(defn log? [object]
  (= (::type (meta object)) :log))

(defn type [object]
  (::type (meta object)))

(defn code->node [code]
  (condp = (.substring code 0 2)
    "OK" "opencache.uk"
    "OC" "opencaching.de"))

(def filter-geocache-transform
  (filter geocache?))

(def filter-raw-geocache-transform
  (filter #(= (:object_type %1) "geocache")))

(def filter-active-geocache-transform
  (filter #(= (:status %1) "Available")))

(def process-part-transform
  (mapcat
    (fn [part-file]
      (logging/report {:stage :reading-file :path part-file})
      (with-open [is (fs/input-stream part-file)]
        (json/read-keyworded is)))))

(def full-dump-transform
  (comp
    process-part-transform
    format-object-transform))

(def geocache-dump-transform
  (comp
    process-part-transform
    filter-raw-geocache-transform
    format-object-transform
    filter-active-geocache-transform))

(defn full-dump-parts [metadata]
  (map
    (partial path/child (:path metadata))
    (:data_files metadata)))

(defn full-dump-seq [metadata]
  ; opens first 32 files at first access
  ;(mapcat
  ;  (fn [part-file]
  ;    (logging/report {:stage :reading-file :path part-file})
  ;    (with-open [is (fs/input-stream
  ;                     (path/child
  ;                       (:path full-dump-metadata)
  ;                       part-file))]
  ;      (json/read-keyworded is)))
  ;  (:data_files full-dump-metadata)))

  ; using transducers
  (sequence
    full-dump-transform
    (full-dump-parts metadata)))
