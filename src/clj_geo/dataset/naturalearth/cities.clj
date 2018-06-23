(ns clj-geo.dataset.naturalearth.cities
  (:require
    [clj-common.path :as path]
    [clj-geo.env :as env]
    [clj-geo.import.shp :as shp]))

; http://www.naturalearthdata.com

(defn create-cities
  "Uses ne_10m_populated_places dataset to extract cities"
  ^{
     :dataset ["ne_10m_populated_places"]}
  []
  (let [path (path/child
               env/*dataset-path*
               "naturalearthdata.com"
               "ne_10m_populated_places"
               "ne_10m_populated_places.shp")]
    (let [features (shp/load-file-data-store path)]
      (filter
        #(= (:featurecla %1) :continent)
        (map
          shp/feature->map
          features)))))

(comment
  (def file (shp/load-file-data-store
              (path/child
                env/*dataset-path*
                "naturalearthdata.com"
                "ne_10m_populated_places"
                "ne_10m_populated_places.shp")))

  (def city (first (filter #(= (:name %) "Belgrade") (map shp/feature->map file))))

  (count (map shp/feature->map file))

  (meta city)

  city)
