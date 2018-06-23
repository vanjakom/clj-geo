(ns clj-geo.env
  (:require
    [clj-common.jvm :as jvm]))

(require '[clj-common.path :as path])

(def ^:dynamic *dataset-path*
  ["Users" "vanja" "projects" "clj-geo" "data"])

(def ^:dynamic *tile-cache-path*
  ["Users" "vanja" "projects" "clj-geo" "data" "tile-cache"])

(def ^:dynamic *thunderforest-apikey*
  (jvm/environment-variable "THUNDERFOREST_APIKEY"))
