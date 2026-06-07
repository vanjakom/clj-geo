(ns clj-geo.dot.process.track
  (:require
   [clj-common.as :as as]
   [clj-common.io :as io]
   [clj-common.localfs :as fs]
   [clj-common.path :as path]))

(defn extract-track
  "Assumes sorted dot seq on input, map with :longitude, :latitude and :tags,
   creates geojson containing LineString."
  [dot-seq]
  
  )
