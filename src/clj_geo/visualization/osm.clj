(ns clj-geo.visualization.osm
  (:require
    [clj-common.2d :as draw]))

(defn render-tiles
  "Renders given set of tiles with retrieved image data to minimal surface"
  [tile-data-seq]
  (let [[x-min x-max y-min y-max] (reduce
                                    (fn [[x-min x-max y-min y-max] tile]
                                      [
                                        (min (or x-min (:x tile)) (:x tile))
                                        (max (or x-max (:x tile)) (:x tile))
                                        (min (or y-min (:y tile)) (:y tile))
                                        (max (or y-max (:y tile)) (:y tile))])
                                    [nil nil nil nil]
                                    tile-data-seq)
        x-span (inc (Math/abs (- x-max x-min)))
        y-span (inc (Math/abs (- y-max y-min)))
        image-context (draw/create-image-context (* 256 x-span) (* 256 y-span))
        graphics (.getGraphics image-context)]
    (draw/write-background
      image-context
      draw/color-white)
    (doseq [tile tile-data-seq]
      (let [image (draw/input-stream->image (:data tile))]
        (.drawImage
          graphics
          image
          (* (- (:x tile) x-min) 256)
          (* (- (:y tile) y-min) 256)
          256
          256
          nil)))
    image-context))
