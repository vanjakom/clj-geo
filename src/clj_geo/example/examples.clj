(ns clj-geo.example.examples)

(require 'clj-geo.visualization.raster)
(require 'clj-geo.visualization.background)
(require 'clj-geo.env)
(require 'clj-common.2d)
(require 'clj-common.localfs)

#_(with-open [os (clj-common.localfs/output-stream ["tmp" "continents.bmp"])]
  (binding [clj-geo.visualization.raster/*width* 1800
            clj-geo.visualization.raster/*height* 900]
    (let [continent-image (clj-geo.visualization.background/create-black-continents-background)]
     (clj-common.2d/write-to-stream
      continent-image
      os))))

#_(with-open [os (clj-common.localfs/output-stream ["tmp" "countries.bmp"])]
  (binding [clj-geo.visualization.raster/*width* 1000
            clj-geo.visualization.raster/*height* 500]
    (let [continent-image (clj-geo.visualization.background/create-black-region-countries-background)]
     (clj-common.2d/write-to-stream
      continent-image
      os))))
