(ns clj-geo.env)

(require '[clj-common.path :as path])

(def ^:dynamic *dataset-path*
  (path/string->path
    "/Users/vanja/projects/clj-geo/data"))
