(ns clj-geo.visualization.raster)

(require 'clj-geo.dataset.naturalearth.continents)
(require 'clj-common.path)
(require 'clj-common.localfs)
(require 'clj-common.2d)

(defn location->point
  "Tranforms location into point with coord system zero in upper left scaled to fit
  point space"
  [point-space-width point-space-height location]
  (let [normalized-longitude (+ (:longitude location) 180)
        normalized-latitude (* (- (:latitude location) 90) -1)]
    {
      :x (int (/ (* normalized-longitude point-space-width) 360))
      :y (int (/ (* normalized-latitude point-space-height) 180))}))

(defn create-continent-image []
  (let [continents (clj-geo.dataset.naturalearth.continents/create-continents)
        image-context (clj-common.2d/create-image-context 1800 900)
        location-transform-fn (partial location->point 1800 900)]
    (clj-common.2d/write-background image-context clj-common.2d/color-white)
    (doseq [continent continents]
      (doseq [geometry (:the_geom continent)]
        (clj-common.2d/fill-poly
          image-context
          (map
            location-transform-fn
            geometry)
          clj-common.2d/color-black)))
    image-context))

