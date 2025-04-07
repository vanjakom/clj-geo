(ns clj-geo.dataset.naturalearth.continents
  (:require
    [clj-common.path :as path]
    [clj-geo.env :as env]
    [clj-geo.import.shp :as shp]))

; http://www.naturalearthdata.com

(defn normalize-feature-class [feature-map]
  (assoc
    feature-map
    :featurecla
    (condp = (:featurecla feature-map)
      "Basin" :basin
      "Plateau" :plateau
      "Island group" :island-group
      "Tundra" :tundra
      "Isthmus" :isthmus
      "Desert" :desert
      "Island" :island
      "Peninsula" :peninsula
      "Pen/cape" :cape
      "Continent" :continent
      "Range/mtn" :mountain-range
      "Geoarea" :geoarea
      "Plain" :plain
      :else (:featurecla feature-map))))

(defn normalize-continent-name [feature-map]
  (if (= (:featurecla feature-map) :continent)
    (assoc
      feature-map
      :name
      (condp = (:name feature-map)
        "SOUTH AMERICA" "South America"
        "AUSTRALIA" "Australia"
        "AFRICA" "Africa"
        "ANTARCTICA" "Antarctica"
        "ASIA" "Asia"
        "EUROPE" "Europe"
        "NORTH AMERICA" "North America"
        :else (:name feature-map)))
    feature-map))

(defn create-continents
  "Uses 110m geography regions dateset to filter out continents."
  ^{
     :dataset ["ne_110m_geography_regions_polys"]}
  []
  (let [path (path/child
               env/*dataset-path*
               "naturalearthdata.com"
               "ne_110m_geography_regions_polys"
               "ne_110m_geography_regions_polys.shp")]
    (let [features (shp/load-file-data-store path)]
      (filter
        #(= (:featurecla %1) :continent)
        (map
          (comp
            normalize-continent-name
            normalize-feature-class
            shp/feature->map)
          features)))))

