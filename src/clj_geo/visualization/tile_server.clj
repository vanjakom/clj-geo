(ns clj-geo.visualization.tile-server
  "Provides simple HTTP sever which should be able to serve tiles for other devices and
  provide web page for debug"
  (:require
   [clj-common.localfs :as fs]
   [clj-common.io :as io]
   [clj-common.path :as path]
   [clj-common.jvm :as jvm]
   [clj-common.logging :as logging]
   [clj-common.2d :as draw]
   [clj-common.http-server :as server]
   compojure.core))

(defn create-handler
  [path]
  (compojure.core/routes
   (compojure.core/GET
    "/explore/:zoom/:longitude/:latitude" [zoom longitude latitude]
    {
     :status 200
     :headers {
               "ContentType" "text/html"}
     :body
     (str
      "<html>"
      "<head>"
      "<title>Map</title>"
      "<style \"type\"=\"text/css\">html,body,#map {width:100%;height:100%;margin:0;}</style>"
      "<script src=\"http://cdnjs.cloudflare.com/ajax/libs/openlayers/2.11/OpenLayers.js\">"
      "</script>"
      "<script>"
      "function init() {"
      "map = new OpenLayers.Map(\"map\");"
      "var layer = new OpenLayers.Layer.OSM("
      "\"layer\",\"/tile/${z}/${x}/${y}\",{numZoomLevels:19});"
      "var from = new OpenLayers.Projection(\"EPSG:4326\");"
      "var to = new OpenLayers.Projection(\"EPSG:900913\");"
      "var position = new OpenLayers.LonLat(" longitude "," latitude ").transform(from, to);"
      "map.addLayer(layer);"
      "map.setCenter(position," zoom ");"
      "}"
      "</script>"
      "<head>"
      "<body onload=\"init();\">"
      "<div id=\"map\"></div>"
      "</body>"
      "</html>")})
   (compojure.core/GET
    "/tile/:zoom/:x/:y" [zoom x y]
    (logging/report {:zoom zoom :x x :y y})
    (let [tile-path (path/child path zoom x y)]
      (if (fs/exists? tile-path)
        {
         :status 200
         :headers {
                   "ContentType" "image/png"}
         :body (fs/input-stream tile-path)}
        (let [context (draw/create-image-context 256 256)]
          (draw/write-background context draw/color-white)
          (draw/draw-text context draw/color-black (str zoom "/" x "/" y) 20 20)
          {
           :status 200
           :headers {
                     "ContentType" "image/png"}
           :body (let [buffer-output-stream (io/create-buffer-output-stream)]
                   (draw/write-png-to-stream context buffer-output-stream)
                   (io/buffer-output-stream->input-stream buffer-output-stream))}))))
   (compojure.core/GET
    "/status"
    _
    (fn [request]
      {:status 200
       :body "ok\n"}))))

(defn create-tile-server
  "Creates http server for tiles from given path"
  [port path]
  (server/create-server port (create-handler path)))
