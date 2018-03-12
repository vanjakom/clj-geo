(ns clj-geo.visualization.geotools)

(require '[clj-common.path :as path])

(import org.geotools.data.FileDataStoreFinder)
(import org.geotools.map.MapContent)
(import org.geotools.map.FeatureLayer)
(import org.geotools.swing.JMapFrame)
(import org.geotools.styling.SLD)

(defn show-shp [path]
  (let [data-store (FileDataStoreFinder/getDataStore
                     (new
                       java.io.File
                       (path/path->string path)))
        feature-source (.getFeatureSource data-store)
        map-content (new MapContent)
        layer (new
                FeatureLayer
                feature-source
                (SLD/createSimpleStyle (.getSchema feature-source)))]
    (.addLayer map-content layer)
    (JMapFrame/showMap map-content)))
