(ns clj-geo.dataset.opencachingde)

(require '[clj-common.path :as path])
(require '[clj-geo.import.okapi :as okapi])
(require '[clj-geo.env :as env])
(require '[clj-common.logging :as logging])

(require '[clj-common.reducers :as r])

(def ^:const full-dump-path ["opencaching.de"])

(defn full-dump-metadata []
  (okapi/full-dump-metadata
    (apply
      path/child
      env/*dataset-path*
      full-dump-path)))

(defn full-dump-seq []
  (okapi/full-dump-seq
    (full-dump-metadata)))
