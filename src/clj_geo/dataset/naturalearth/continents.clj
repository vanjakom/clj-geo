(ns clj-geo.dataset.naturalearth.continents)

(require '[clj-common.path :as path])

(require '[clj-geo.env :as env])

(import org.geotools.data.FileDataStoreFinder)

(defn load-file-data-store [path]
  (let [datastore-finder (FileDataStoreFinder/getDataStore
                           (new java.io.File (path/path->string path)))
        feature-source (.getFeatureSource datastore-finder)]
    (.toArray (.getFeatures feature-source))))

(defn feature->map [feature]
  (apply
    merge
    (map
      (fn [property]
        {
          (keyword (.getLocalPart (.getName property)))
          (.getValue property)})
      (.getProperties feature))))

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
          (map
            (fn [coordinate]
              {:longitude (.x coordinate) :latitude (.y coordinate)})
            (.getCoordinates (.getGeometryN poly geometry-index))))
        (range (.getNumGeometries poly))))))

(defn normalize-feature-class [feature-map]
  (assoc
    feature-map
    :featurecla
    (condp = (:featurecla feature-map)
      "Basin" :basin
      "Plateau" :plateau
      "Island group" :island-group
      "Tundra" :tundra
      "Isthmus" :isthmus
      "Desert" :desert
      "Island" :island
      "Peninsula" :peninsula
      "Pen/cape" :cape
      "Continent" :continent
      "Range/mtn" :mountain-range
      "Geoarea" :geoarea
      "Plain" :plain
      :else (:featurecla feature-map))))

(defn normalize-continent-name [feature-map]
  (if (= (:featurecla feature-map) :continent)
    (assoc
      feature-map
      :name
      (condp = (:name feature-map)
        "SOUTH AMERICA" "South America"
        "AUSTRALIA" "Australia"
        "AFRICA" "Africa"
        "ANTARCTICA" "Antarctica"
        "ASIA" "Asia"
        "EUROPE" "Europe"
        "NORTH AMERICA" "North America"
        :else (:name feature-map)))
    feature-map))

(defn create-continents
  "Uses 110m geography regions dateset to filter out continents."
  ^{
     :dataset ["ne_110m_geography_regions_polys"]}
  []
  (let [path (path/child
               env/*dataset-path*
               "naturalearthdata.com"
               "ne_110m_geography_regions_polys"
               "ne_110m_geography_regions_polys.shp")]
    (let [features (load-file-data-store path)]
      (filter
        #(= (:featurecla %1) :continent)
        (map
          (comp
            normalize-continent-name
            normalize-feature-class
            replace-geom
            append-area
            feature->map)
          features)))))

