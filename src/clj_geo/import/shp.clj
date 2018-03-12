(ns clj-geo.import.shp)

(require '[clj-common.path :as path])

(import org.geotools.data.FileDataStoreFinder)
(import com.vividsolutions.jts.geom.Polygon)

(defn load-file-data-store [path]
  (let [datastore-finder (FileDataStoreFinder/getDataStore
                           (new java.io.File (path/path->string path)))
        feature-source (.getFeatureSource datastore-finder)]
    (.toArray (.getFeatures feature-source))))

(defn feature->map [feature]
  (with-meta
    (apply
      merge
      (map
        (fn [property]
          {
            (keyword (.getLocalPart (.getName property)))
            (.getValue property)})
        (.getProperties feature)))
    {
      :feature-class (.getClass feature)}))

(defn append-area [feature-map]
  (assoc
    feature-map
    :area
    (.getArea (:the_geom feature-map))))

(defn replace-geom [feature-map]
  (let [poly (:the_geom feature-map)]
    (assoc
      feature-map
      :the_geom
      (map
        (fn [geometry-index]
          (let [geometry (.getGeometryN poly geometry-index)]
            (with-meta
              (if (instance? Polygon geometry)
                (map
                  (fn [coordinate]
                    {:longitude (.x coordinate) :latitude (.y coordinate)})
                  (.getCoordinates (.getExteriorRing geometry)))
                (map
                  (fn [coordinate]
                    {:longitude (.x coordinate) :latitude (.y coordinate)})
                  (.getCoordinates geometry)))
              {
                :geometry-class (.getClass geometry) :num-geometries (.getNumGeometries geometry)})))
        (range (.getNumGeometries poly))))))
