(ns clj-geo.dataset.geonames.cities
  (:require
    [clj-common.path :as path]
    [clj-common.io :as io]
    [clj-common.localfs :as fs]
    [clj-geo.env :as env]))

(defn name->keyword [name]
  (keyword
    (.toLowerCase
      (.replace
        (.trim name)
        " "
        "-"))))

(defn optional-long [value]
  (if-let [value value]
    (Long/parseLong value)))

(defn optional-double [value]
  (if-let [value value]
    (Double/parseDouble value)))

(def geoname-header
  [
    [(name->keyword "geonameid") optional-long]
    [(name->keyword "name") identity]
    [(name->keyword "asciiname") identity]
    [(name->keyword "alternatenames") identity]
    [(name->keyword "latitude") optional-double]
    [(name->keyword "longitude") optional-double]
    [(name->keyword "feature class") identity]
    [(name->keyword "feature code") identity]
    [(name->keyword "country code") identity]
    [(name->keyword "cc2") identity]
    [(name->keyword "admin1 code") identity]
    [(name->keyword "admin2 code") identity]
    [(name->keyword "admin3 code") identity]
    [(name->keyword "admin4 code") identity]
    [(name->keyword "population") optional-long]
    [(name->keyword "elevation") identity]
    [(name->keyword "dem") identity]
    [(name->keyword "timezone") identity]
    [(name->keyword "modification date") identity]])

(defn tsv-line->map [header line]
  (let [parts (.split line "\t")]
    (zipmap header parts)))

(defn tsv-line->typed-map [header-type line]
  (let [parts (.split line "\t")]
    (zipmap
      (map first header-type)
      (map
        (fn [type-fn value]
          (type-fn value))
        (map second header-type)
        parts))))

(comment
  (tsv-line->typed-map
    [[:id optional-long] [:name identity]]
    "10\tname"))


(defn tsv-line->header [line]
  (map
    keyword
    (map
      #(.toLowerCase %)
      (.split line "\t"))))

(defn create-cities-1000 []
  (with-open [cities-1000-stream (fs/input-stream
                                   (path/child
                                     env/*dataset-path*
                                     "geonames.org"
                                     "GazetteerData"
                                     "cities1000.txt"))]
    (let [lines (line-seq (io/input-stream->buffered-reader cities-1000-stream))]
      (doall
        (map
          (partial
            tsv-line->typed-map
            geoname-header)
          lines)))))

(comment
  (count (create-cities-1000))
  (select-keys
    (first (filter #(= (:name %) "Belgrade") (create-cities-1000)))
    [:latitude :longitude]))
