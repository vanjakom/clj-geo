(ns clj-geo.dataset.thematicmapping.countries
  (:require
    [clj-common.path :as path]
    [clj-geo.env :as env]
    [clj-geo.import.shp :as shp]))

; http://thematicmapping.org/downloads/

(defn create-countries
  "Uses country borders dataset to filter out continents."
  ^{
     :dataset ["TM_WORLD_BORDERS-0"]}
  []
  (let [path (path/child
               env/*dataset-path*
               "thematicmapping.org"
               "TM_WORLD_BORDERS-0"
               "TM_WORLD_BORDERS-0.3.shp")]
    (let [features (shp/load-file-data-store path)]
      (map
        shp/feature->map
        features))))

(comment
  (first (:the_geom (first (create-countries)))))
