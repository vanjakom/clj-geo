(ns clj-geo.example.examples)

(require 'clj-common.localfs)
(require 'clj-common.2d)

(require 'clj-geo.visualization.raster)

(defn write-continent-image []
  (let [continent-image (clj-geo.visualization.raster/create-continent-image)]
    (clj-common.2d/write-to-stream
      continent-image
      (clj-common.localfs/output-stream ["tmp" "continents.bmp"]))))

