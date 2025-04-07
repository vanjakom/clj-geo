(ns clj-geo.example.osm-based-map
  (:require
    ring.middleware.stacktrace
    ring.middleware.params
    ring.middleware.keyword-params

    compojure.core
    [clj-common.as :as as]
    clj-common.io
    clj-common.2d
    clj-common.http-server
    clj-common.ring-middleware
    clj-geo.import.osm
    clj-geo.visualization.osm))


; usage
; lein run clj-geo.example.osm-based-map
;
; sample request
; 20.37500 20.50357
; 44.76453 44.86256
; Belgrade, Serbia
; http://localhost:7079/render?tile-set=stamen-lines&zoom=10&min-longitude=20.37500&max-longitude=20.50357&min-latitude=44.76453&max-latitude=44.86256
; http://localhost:7079/render?tile-set=stamen-lines&zoom=14&min-longitude=20.37500&max-longitude=20.50357&min-latitude=44.76453&max-latitude=44.86256


(defn render-handler
  [
    {
      {
        min-longitude :min-longitude
        max-longitude :max-longitude
        min-latitude :min-latitude
        max-latitude :max-latitude

        zoom :zoom
        tile-set :tile-set} :params :as request}]

  {
    :headers {"Content-Type" "image/png"}
    :status 200
    :body (let [output-stream (clj-common.io/create-buffer-output-stream)]
            (clj-common.2d/write-png-to-stream
              (clj-geo.visualization.osm/render-tiles
                (clj-geo.import.osm/retrieve-tile-data-seq
                  (as/keyword tile-set)
                  (as/integer zoom)
                  (as/double min-longitude)
                  (as/double max-longitude)
                  (as/double min-latitude)
                  (as/double max-latitude)))
              output-stream)
            (clj-common.io/buffer-output-stream->input-stream output-stream))})

(defn -main [& args]
  (clj-common.http-server/create-server
    7079
    (compojure.core/routes
      (compojure.core/GET
        "/render"
        _
        (ring.middleware.params/wrap-params
          (ring.middleware.keyword-params/wrap-keyword-params
            (ring.middleware.stacktrace/wrap-stacktrace-log
              (var render-handler)))))))
  (println "Server started on 7079"))

(comment
  (-main)

  (clj-common.http-server/stop-server 7079))
