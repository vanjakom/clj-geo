(ns clj-geo.import.shp)

(require '[clj-common.path :as path])

(import org.geotools.data.FileDataStoreFinder)

(defn load-file-data-store [path]
  (let [datastore-finder (FileDataStoreFinder/getDataStore
                           (new java.io.File (path/path->string path)))
        feature-source (.getFeatureSource datastore-finder)]
    (.toArray (.getFeatures feature-source))))

(defn parse-geom [value]
  (map
    (fn [geometry-index]
      (let [geometry (.getGeometryN value geometry-index)]
        (with-meta
          (if (instance? org.locationtech.jts.geom.Polygon geometry)
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
    (range (.getNumGeometries value))))

(defn deserialize-value [key value]
  (condp = key
    :the_geom (parse-geom value)
    (condp = (type value)
      nil nil
      Integer value
      Long value
      Double value
      Boolean value
      String value
      org.locationtech.jts.geom.Polygon {:x (.getX value) :y (.getY value)}
      (str (.getName (.getClass value)) " - " (.toString value)))))

(comment
  (deserialize-value "test")

  (deserialize-value (new java.net.URI "http://test.com")))

(defn feature->map [feature]
  (with-meta
    (apply
      merge
      (map
        (fn [property]
          (let [key (keyword (.toLowerCase (.getLocalPart (.getName property))))]
            {
              key
              (deserialize-value key (.getValue property))}))
        (.getProperties feature)))
    {
      :feature-class (.getClass feature)}))
