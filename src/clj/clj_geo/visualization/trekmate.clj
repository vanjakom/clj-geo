(ns clj-geo.visualization.trekmate
  (:use
    clj-common.clojure
    clj-common.test)
  (:require
    [clj-common.2d :as draw]
    [clj-common.logging :as logging]
    [clj-geo.import.osm :as osm-import]
    [clj-geo.visualization.osm :as osm-render]
    [maply-backend-tools.data.pins :as pins]))

(comment

; custom drawing logic / some of fns could go to clj-common

(defn draw-line [set-point-fn point-seq]
  (doseq [[[x1 y1] [x2 y2]]
          (partition 2 1 point-seq)]
    (let [dx (- x2 x1)
          dy (- y2 y1)]
      (if (not (and (= dx 0) (= dy 0)))
        (if (or (= dx 0) (> (Math/abs (float (/ dy dx))) 1))
          (if (< dy 0)
            (doseq [y (range y2 (inc y1))]
              (set-point-fn (+ x1 (/ (* (- y y1) dx) dy)) y))
            (doseq [y (range y1 (inc y2))]
              (set-point-fn (+ x1 (/ (* (- y y1) dx) dy)) y)))
          (if (< dx 0)
            (doseq [x (range x2 (inc x1))]
              (set-point-fn x (+ y1 (* (/ dy dx) (- x x1)))))
            (doseq [x (range x1 (inc x2))]
              (set-point-fn x (+ y1 (* (/ dy dx) (- x x1)))))))))))

(defn draw-point [set-point-fn [point-x point-y] radius]
  (doseq [x (range (- point-x radius) (+ point-x radius 1))]
    (doseq [y (range (- point-y radius) (+ point-y radius 1))]
      (set-point-fn x y))))

(defn create-java-context
  "Creates image context that could be used in pair with create-set-point-java."
  [resolution min-longitude max-longitude min-latitude max-latitude]
  (let [image-context-width (int (Math/ceil (* (- max-longitude min-longitude) resolution)))
        image-context-height (int (Math/ceil (* (- max-latitude min-latitude) resolution)))]
    (draw/create-image-context
      image-context-width
      image-context-height)))

(defn create-set-point-java
  "Performs coordinate normalization. Assumes x, y will be in lower left coordinate system"
  [context color]
  (let [width (draw/context-width context)
        height (draw/context-height context)]
    (fn [x y]
      (if
        (and (>= x 0) (< x width) (>= y 0) (< y height))
        (draw/set-point context color x (- height y 1))))))

(defn create-set-point-raw-java
  "Without coordinate normalization"
  [context color]
  (let [width (draw/context-width context)
        height (draw/context-height context)]
    (fn [x y]
      (if
        (and (>= x 0) (< x width) (>= y 0) (< y height))
        (draw/set-point context color x y)))))

(defn create-set-point-with-radius [base-set-point-fn radius]
  (fn [point-x point-y]
    (doseq [x (range (- point-x radius) (+ point-x radius 1))]
      (doseq [y (range (- point-y radius) (+ point-y radius 1))]
        (base-set-point-fn x y)))))

(defn create-location->point
  "Creates transform fn which transforms longitude, latitude to point,
  taking longitude, latitude offset and resolution into account.
  resolution - how many pixels will be used to render 1 degree"
  [resolution min-longitude max-longitude min-latitude max-latitude]
  (fn [{longitude :longitude latitude :latitude}]
    [(int (* (- longitude min-longitude) resolution))
     (int (* (- latitude min-latitude) resolution))]))

; pin matching functions

(todo-warn "implement default pin ...")
(def default-pin nil)

; rendering functions

(defn render-on-tile [configuration tile routes locations]
  (let [route-thickness (or (:route-thickness configuration) 1)
        route-color (or (:route-color configuration) draw/color-red)
        background-context (draw/input-stream->image (:data tile))
        pin-match-fn (or (:pin-match-fn configuration) pins/calculate-pins)
        pin-load-fn (cond
                      (= (:pin-size configuration) :small) pins/load-small-pin
                      (= (:pin-size configuration) :medium) pins/load-medium-pin
                      (= (:pin-size configuration) :large) pins/load-large-pin
                      :else pins/load-medium-pin)
        location-convert-fn (fn [location]
                              (let [[x y] (osm-import/calculate-x-y (:zoom tile) location)]
                                [
                                  (int (- x (* (:x tile) 256)))
                                  (int (- y (* (:y tile) 256)))]))
        route-set-point-fn (create-set-point-with-radius
                             (create-set-point-raw-java
                               background-context route-color)
                             route-thickness)
        ; to be used when drawing pins to ensure pin is inside image
        location-test-fn (fn [[x y]]
                           (and
                             (> x 0)
                             (> y 0)
                             (< x (draw/context-width background-context))
                             (< y (draw/context-height background-context))))]
    (doseq [route routes]
      (draw-line
        route-set-point-fn
        (map
          location-convert-fn
          (:locations route))))
    (doseq [location locations]
      (let [point (location-convert-fn location)]
        (logging/report {:location location :point point})
        (if (location-test-fn point)
          (do
            (logging/report {:location location})
            (if-let [pin (first (pin-match-fn (:tags location)))]
              (do
                (logging/report {:location location :point point :pin pin})
                (draw/draw-image
                  background-context
                  point
                  (pin-load-fn pin))))))))
    background-context))

(defn render-osm-map [configuration zoom tile-data-seq routes locations]
  (let [route-thickness (or (:route-thickness configuration) 1)
        route-color (or (:route-color configuration) draw/color-red)
        background-context (osm-render/render-tiles tile-data-seq)
        pin-match-fn (or (:pin-match-fn configuration) pins/calculate-pins)
        pin-load-fn (cond
                      (= (:pin-size configuration) :small) pins/load-small-pin
                      (= (:pin-size configuration) :medium) pins/load-medium-pin
                      (= (:pin-size configuration) :large) pins/load-large-pin
                      :else pins/load-medium-pin)
        [x-min x-max y-min y-max] (reduce
                                    (fn [[x-min x-max y-min y-max] tile]
                                      [
                                        (min (or x-min (:x tile)) (:x tile))
                                        (max (or x-max (:x tile)) (:x tile))
                                        (min (or y-min (:y tile)) (:y tile))
                                        (max (or y-max (:y tile)) (:y tile))])
                                    [nil nil nil nil]
                                    tile-data-seq)
        offset-x (* x-min 256)
        offset-y (* y-min 256)
        location-convert-fn (fn [location]
                              (let [[x y] (osm-import/calculate-x-y zoom location)]
                                [
                                  (int (- x offset-x))
                                  (int (- y offset-y))]))
        route-set-point-fn (create-set-point-with-radius
                             (create-set-point-raw-java
                               background-context route-color)
                             route-thickness)
        ; to be used when drawing pins to ensure pin is inside image
        location-test-fn (fn [[x y]]
                           (and
                             (> x 0)
                             (> y 0)
                             (< x (draw/context-width background-context))
                             (< y (draw/context-height background-context))))]
    (doseq [route routes]
      (draw-line
        route-set-point-fn
        (map
          location-convert-fn
          (:locations route))))
    (doseq [location locations]
      (let [point (location-convert-fn location)]
        ;(logging/report {:location location :point point})
        (if (location-test-fn point)
          (do
            ;(logging/report {:location location})
            (if-let [pin (first (pin-match-fn (:tags location)))]
              (do
                ;(logging/report {:location location :point point :pin pin})
                (draw/draw-image
                  background-context
                  point
                  (pin-load-fn pin))))))))
    background-context))

(defn plot
  "resolution - how many pixels will 1 degree of longitude / latitude take"
  [resolution min-longitude max-longitude min-latitude max-latitude locations]
  (let [location->point (create-location->point
                          resolution min-longitude max-longitude min-latitude max-latitude)
        image-context (create-java-context
                        resolution min-longitude max-longitude min-latitude max-latitude)
        set-point-fn (create-set-point-java
                       image-context
                       draw/color-white)]
    (draw/write-background image-context draw/color-black)
    (draw-line
      set-point-fn
      (map
        location->point
        locations))
    image-context))
 )
