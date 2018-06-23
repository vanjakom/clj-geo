(ns clj-geo.dataset.techslides.capitals
  (:require
    [clj-common.path :as path]
    [clj-common.localfs :as fs]
    [clj-common.io :as io]
    [clj-common.json :as json]
    [clj-common.http :as http]
    [clj-geo.env :as env]))


; data-set obtained from:
; http://techslides.com/list-of-countries-and-capitals
; source file
; http://techslides.com/demos/country-capitals.json

; to be used only once to generate dataset

(defn generate []
  (let [path (path/child
               env/*dataset-path*
               "techslides.com"
               "country-capitals.json")]
    (with-open [input-stream (http/get-as-stream
                               "http://techslides.com/demos/country-capitals.json")
                 output-stream (fs/output-stream path)]
      (io/copy-input-to-output-stream input-stream output-stream))))

; to be used when playing with dataset

(defn create-capitals []
  (let [path (path/child
               env/*dataset-path*
               "techslides.com"
               "country-capitals.json")]
    (with-open [input-stream (fs/input-stream path)]
      (doall (json/read-keyworded input-stream)))))

(comment
  (generate)

  (count (create-capitals))
  ;> 245
  )
