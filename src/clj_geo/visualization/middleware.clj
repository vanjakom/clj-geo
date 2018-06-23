(ns clj-geo.visualization.middleware
  (:require
    ring.middleware.params
    ring.middleware.keyword-params

    [clj-common.io :as io]
    [clj-common.2d :as draw]
    clj-common.ring-middleware
    clj-geo.visualization.background
    [clj-geo.visualization.raster :as raster]))


(defn expose-map []
  (clj-common.ring-middleware/wrap-exception-to-logging
    (ring.middleware.params/wrap-params
      (ring.middleware.keyword-params/wrap-keyword-params
        (fn [request]
          (let [namespace (or (:namespace (:params request)) "user")
                name (:name (:params request))
                background (:background (:params request))
                location-point-size (Long/parseLong (or (:location-point-size (:params request)) "10"))]
            (if-let [var (ns-resolve (symbol namespace) (symbol name))]
              (let [background-fn (ns-resolve 'clj-geo.visualization.background (symbol background))
                    image-context ((deref background-fn))]
                (raster/draw-locations
                  {
                    :point-size location-point-size
                    :image-context image-context}
                  (deref var))
                (let [output-stream (io/buffer-output-stream)]
                  (draw/write-png-to-stream image-context output-stream)
                  {
                    :status 200
                    :headers {
                               "Content-Type" "image/png"}
                    :body (io/buffer-output-stream->input-stream output-stream)}))
              {
                :status 404})))))))


(comment
  ; http://localhost:7078/test?namespace=user&name=locations&background=create-black-continents-background&location-point-size=10
  (require 'clj-common.http-server)
  (def server   (clj-common.http-server/create-server
                  7078
                  (compojure.core/GET
                    "/test"
                    _
                    (expose-map))))
  (.stop server))
