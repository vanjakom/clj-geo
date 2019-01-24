(ns clj-geo.env
  (:require
   [clj-common.jvm :as jvm]
   [clj-common.path :as path]))

(def ^:dynamic *dataset-path*
  (path/string->path
   (jvm/environment-variable "CLJ_GEO_DATA")))

(def ^:dynamic *tile-cache-path*
  (path/child
   *dataset-path*
   "tile-cache"))


;;; free APIKEY could be obtained from www.thunderforest.com
(def ^:dynamic *thunderforest-apikey*
  (jvm/environment-variable "THUNDERFOREST_APIKEY"))
