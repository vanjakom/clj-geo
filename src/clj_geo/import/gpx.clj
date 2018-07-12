(ns clj-geo.import.gpx
  (:require [clojure.xml :as xml])
  (:require [clj-common.path :as path])
  (:require [clj-common.localfs :as fs])
  (:require [clj-common.view :as view]))


(defn read [path]
  (xml/parse (fs/input-stream path)))

(defn read-stream [input-stream]
  (xml/parse input-stream))

; understand format and prepare default schema, currently I'm reading only geocaching.com gpx

