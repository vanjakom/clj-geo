(ns clj-geo.example.examples)

(require 'clj-common.localfs)
(require 'clj-common.2d)

(require 'clj-geo.visualization.raster)

(require 'clj-geo.dataset.naturalearth.continents)
(require 'clj-geo.dataset.thematicmapping.countries)

(defn write-continent-image []
  (let [continent-image (clj-geo.visualization.raster/feature-seq->image
                          (clj-geo.dataset.naturalearth.continents/create-continents))]
    (clj-common.2d/write-to-stream
      continent-image
      (clj-common.localfs/output-stream ["tmp" "continents.bmp"]))))

(defn write-countries-image []
  (let [continent-image (clj-geo.visualization.raster/feature-seq->image
                          (clj-geo.dataset.thematicmapping.countries/create-countries))]
    (clj-common.2d/write-to-stream
      continent-image
      (clj-common.localfs/output-stream ["tmp" "countries.bmp"]))))

(write-continent-image)
(write-countries-image)

(require '[clj-common.path :as path])

(require '[clj-geo.env :as env])

(require '[clj-geo.import.shp :as shp])

(comment
  (defn create-countries
    "Uses country borders dateset to filter out continents."
    ^{
       :dataset ["TM_WORLD_BORDERS-0"]}
    []
    (let [path (path/child
                 env/*dataset-path*
                 "thematicmapping.org"
                 "TM_WORLD_BORDERS-0"
                 "TM_WORLD_BORDERS-0.3.shp")]
      (let [features (shp/load-file-data-store path)]
        features))))

(comment
  (defn write-canada []
    (let [canada (first
                   (filter
                     #(= (:NAME %1) "Canada")
                     (clj-geo.dataset.thematicmapping.countries/create-countries)))
          problematic-geom (last (take 381 (:the_geom canada)))
          problematic-canada-image (clj-geo.visualization.raster/feature-seq->image (list {:the_geom (list problematic-geom)}))]
      (clj-common.2d/write-to-stream
        problematic-canada-image
        (clj-common.localfs/output-stream ["tmp" "canada.bmp"]))
      problematic-geom)))

