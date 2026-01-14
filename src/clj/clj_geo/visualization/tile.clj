(ns clj-geo.visualization.tile
  (:use
   clj-common.clojure)
  (:require
   [clj-common.2d :as draw]
   [clj-geo.math.tile :as tile]))

(defn render-downscale-tiles
  "From given min and max tile creates all downscale tiles up to zoom level
  ( min-zoom-downscale ) provided. Uses tile-provider-fn to get image context
  with tile at original zoom level and tile-reciever-fn to capture generated
  tile of lower level.
  tile-provider-fn should return null in case specific tile does not exist"
  [tile-provider-fn tile-reciever-fn min-zoom-downscale
   original-min-tile original-max-tile]
  (let [original-zoom (first original-min-tile)]
    (doseq [zoom (range min-zoom-downscale original-zoom)]
      (let [[_ min-x min-y] (first
                             (tile/zoom->tile->tile-seq zoom original-min-tile))
            [_ max-x max-y] (first
                             (tile/zoom->tile->tile-seq zoom original-max-tile))
            width-of-tile (int (* 256 (java.lang.Math/pow
                                       2 (- original-zoom zoom))))]
        (doseq [x (range min-x (inc max-x))]
          (doseq [y (range min-y (inc max-y))]
            (let [image-context (draw/create-image-context
                                 width-of-tile
                                 width-of-tile)
                  tile-seq (tile/zoom->tile->tile-seq original-zoom [zoom x y])
                  [_ base-x base-y] (first tile-seq) ]
              (doseq [tile tile-seq]
                (when-let [tile-context (tile-provider-fn tile)]
                  (let [[_ original-x original-y] tile]
                    (draw/draw-image image-context
                                     [
                                      (+ (* (- original-x base-x) 256) 128)
                                      (+ (* (- original-y base-y) 256) 128)]
                                     tile-context))))
              (let [tile-context (draw/create-thumbnail 256 image-context)]
                (tile-reciever-fn [zoom x y] tile-context)))))))))
