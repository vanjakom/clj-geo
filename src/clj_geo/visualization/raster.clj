(ns clj-geo.visualization.raster
  (:require
    [clj-common.2d :as draw]))


; usage:
; maply-backend-tools.visualization.coverage

; set of functions with some defaults applied in location -> point transformation
; used by trek-mate app for dashboard drawings
; logic:
; data is always rendered to 18000x9000 px image representing whole World, once image
; is ready region could be extracted. by doing things in this way problem with broken
; country, river boundaries is fixed and complexity of transform function propagation
; is removed
; all draw functions assume image-context represents whole world and use widht / height
; from image context

(def ^:dynamic *width* 18000)
(def ^:dynamic *height* 9000)
(def ^:dynamic *color-background* draw/color-white)

(defn location->point
  "Tranforms location into point with coord system zero in upper left scaled to fit
  point space"
  ([location]
   (location->point *width* *height* -180 -90 360 180 location))
  ([point-space-width point-space-height location]
   (location->point point-space-width point-space-height -180 -90 360 180 location))
  ([point-space-width point-space-height longitude-offset latitude-offset longitude-size latitude-size location]
   (let [normalized-longitude (- (:longitude location) longitude-offset)
         normalized-latitude (- (:latitude location) latitude-offset)]
     {
       :x (int (/ (* normalized-longitude point-space-width) longitude-size))
       :y (- point-space-height (int (/ (* normalized-latitude point-space-height) latitude-size)))})))

(defn create-image-context
  ([] (create-image-context {}))
  ([{
      width :width
      height :height
      color-background :color-background
      :or {
            width *width*
            height *height*
            color-background *color-background*}}]
   (let [image-context (draw/create-image-context width height)]
     (draw/write-background image-context color-background)
     image-context)))

(defn extract-image-context [{
                               min-longitude :min-longitude
                               min-latitude :min-latitude
                               max-longitude :max-longitude
                               max-latitude :max-latitude}
                             image-context]

  (let [location-transform-fn (partial
                                location->point
                                (draw/context-width image-context)
                                (draw/context-height image-context))
        {min-x :x min-y :y} (location-transform-fn {:longitude min-longitude :latitude min-latitude})
        {max-x :x max-y :y} (location-transform-fn {:longitude max-longitude :latitude max-latitude})]
    ; note y axel is reverse
    (draw/extract-subimage
      {
        :image-context image-context
        :x min-x
        :y max-y
        :width (- max-x min-x)
        :height (Math/abs (- max-y min-y))})))

(defn draw-poly [image-context color feature-seq]
  (let [location-transform-fn (partial
                                location->point
                                (draw/context-width image-context)
                                (draw/context-height image-context))]
    (doseq [feature feature-seq]
      (doseq [geometry (:the_geom feature)]
        (draw/draw-poly
          image-context
          (map
            location-transform-fn
            geometry)
          color)))
    image-context))

(defn fill-poly [image-context color feature-seq]
  (let [location-transform-fn (partial
                                location->point
                                (draw/context-width image-context)
                                (draw/context-height image-context))]
    (doseq [feature feature-seq]
      (doseq [geometry (:the_geom feature)]
        (draw/fill-poly
          image-context
          (map
            location-transform-fn
            geometry)
          color)))
    image-context))

(defn draw-locations [{
                        image-context :image-context
                        point-size :point-size
                        color :color
                        :or {

                              point-size 1
                              color draw/color-green}}
                      locations]
  (let [location-convert-fn (partial
                              location->point
                              (draw/context-width image-context)
                              (draw/context-height image-context))]
    (doseq [location locations]
      (let [points (if (> point-size 1)
                     (map
                       (fn [[x-offset y-offset]]
                         (draw/offset-point (location-convert-fn location) x-offset y-offset))
                       (mapcat
                         (fn [x]
                           (map
                             (fn [y]
                               [x y])
                             (range (- (/ point-size 2)) (/ point-size 2))))
                         (range (- (/ point-size 2)) (/ point-size 2))))
                     [(location-convert-fn location)])]
        (draw/set-points
          image-context
          points
          color)))))

(defn feature-seq->image [feature-seq]
  (draw-poly
    (create-image-context)
    feature-seq
    draw/color-black))


(comment

  (location->point
    1800 900

    -180 -90

    360 180
    {:longitude 0 :latitude -90})

  (let [location-convert (partial location->point 1800 900 -180 -90 360 180)]
    (and
      (= {:x 900 :y 900} (location-convert {:longitude 0 :latitude -90}))
      (= {:x 0 :y 0} (location-convert {:longitude -180 :latitude 90}))
      (= {:x 1800 :y 900} (location-convert {:longitude 180 :latitude -90}))))


  (location->point
    1800 900
    -15 30
    60 30
    {:longitude -15 :latitude 30})





  ; legacy
  (defn create-continent-image []
    (feature-seq->image
      (clj-geo.dataset.naturalearth.continents/create-continents))))
