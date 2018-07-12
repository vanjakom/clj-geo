(ns clj-geo.dataset.geonames.cities
  (:require
    [clj-common.path :as path]
    [clj-common.io :as io]
    [clj-common.localfs :as fs]
    [clj-geo.env :as env]
    [clj-geo.dataset.geonames.common :as common]))

(def geoname-header
  [
    [(common/name->keyword "geonameid") common/optional-long]
    [(common/name->keyword "name") identity]
    [(common/name->keyword "asciiname") identity]
    [(common/name->keyword "alternatenames") identity]
    [(common/name->keyword "latitude") common/optional-double]
    [(common/name->keyword "longitude") common/optional-double]
    [(common/name->keyword "feature-class") identity]
    [(common/name->keyword "feature-code") identity]
    [(common/name->keyword "country-code") identity]
    [(common/name->keyword "cc2") identity]
    [(common/name->keyword "admin1-code") identity]
    [(common/name->keyword "admin2-code") identity]
    [(common/name->keyword "admin3-code") identity]
    [(common/name->keyword "admin4-code") identity]
    [(common/name->keyword "population") common/optional-long]
    [(common/name->keyword "elevation") identity]
    [(common/name->keyword "dem") identity]
    [(common/name->keyword "timezone") identity]
    [(common/name->keyword "modification-date") identity]])

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
            common/tsv-line->typed-map
            geoname-header)
          lines)))))

(defn create-cities-5000 []
  (with-open [cities-5000-stream (fs/input-stream
                                   (path/child
                                     env/*dataset-path*
                                     "geonames.org"
                                     "GazetteerData"
                                     "cities5000.txt"))]
    (let [lines (line-seq (io/input-stream->buffered-reader cities-5000-stream))]
      (doall
        (map
          (partial
            common/tsv-line->typed-map
            geoname-header)
          lines)))))

(defn create-cities-15000 []
  (with-open [cities-15000-stream (fs/input-stream
                                   (path/child
                                     env/*dataset-path*
                                     "geonames.org"
                                     "GazetteerData"
                                     "cities15000.txt"))]
    (let [lines (line-seq (io/input-stream->buffered-reader cities-15000-stream))]
      (doall
        (map
          (partial
            common/tsv-line->typed-map
            geoname-header)
          lines)))))

(comment
  (filter #(= (:asciiname %) "Malmoe") (create-cities-15000))

  (count (create-cities-5000))


  (count (create-cities-1000))
  (select-keys
    (first (filter #(= (:name %) "Belgrade") (create-cities-1000)))
    [:latitude :longitude]))
