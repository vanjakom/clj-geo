(ns clj-geo.visualization.background
  (:require
    [clj-common.2d :as draw]
    [clj-geo.dataset.thematicmapping.countries :as dataset-countries]
    [clj-geo.dataset.naturalearth.continents :as dataset-continents]
    [clj-geo.visualization.raster :as raster]))


(defn create-black-continents-background
  ([]
   (create-black-continents-background {}))
  ([{
      width :width
      height :height
      :or {
            width raster/*width*
            height raster/*height*}}]
   (let [image-context (raster/create-image-context
                         {
                           :width width
                           :height height})
         continents (dataset-continents/create-continents)]
     (raster/fill-poly image-context draw/color-black continents)
     (raster/draw-poly image-context draw/color-white continents)
     image-context)))


(defn create-black-region-countries-background
  ([]
   (create-black-region-countries-background {}))
  ([{
      width :width
      height :height
      :or {
            width raster/*width*
            height raster/*height*}}]
   (let [image-context (raster/create-image-context
                         {
                           :width width
                           :height height})
         countries (dataset-countries/create-countries)]
     (raster/fill-poly image-context draw/color-black countries)
     (raster/draw-poly image-context draw/color-white countries)
     image-context)))



