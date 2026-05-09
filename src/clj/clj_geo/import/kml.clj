(ns clj-geo.import.kml
  (:require
   [clojure.string :as str]
   [clojure.xml :as xml]
   [clj-geo.import.geojson :as geojson])
  (:import
   [java.io ByteArrayInputStream ByteArrayOutputStream]
   [java.util.zip ZipInputStream]))

(defn- find-tag [elements tag]
  (first (filter #(= (:tag %) tag) elements)))

(defn- get-text [element]
  (first (:content element)))

(defn- parse-coordinates [coord-str]
  (->> (str/split (str/trim coord-str) #"\s+")
       (remove str/blank?)
       (map (fn [coord]
              (let [parts (str/split coord #",")]
                {:longitude (Double/parseDouble (nth parts 0))
                 :latitude (Double/parseDouble (nth parts 1))})))))

(defn- placemark->feature [placemark-content]
  (let [name-el     (find-tag placemark-content :name)
        desc-el     (find-tag placemark-content :description)
        name        (when name-el (get-text name-el))
        description (when desc-el (get-text desc-el))
        properties  (cond-> {}
                      name        (assoc :name name)
                      description (assoc :description description))
        point-el    (find-tag placemark-content :Point)
        linestr-el  (find-tag placemark-content :LineString)]
    (cond
      point-el
      (let [coord-str              (get-text (find-tag (:content point-el) :coordinates))
            {:keys [longitude latitude]} (first (parse-coordinates coord-str))]
        (geojson/point longitude latitude properties))

      linestr-el
      (let [coord-str (get-text (find-tag (:content linestr-el) :coordinates))
            locations (parse-coordinates coord-str)]
        (geojson/line-string properties locations))

      :else nil)))

(defn- collect-features [elements]
  (mapcat
   (fn [el]
     (case (:tag el)
       :Placemark (filter some? [(placemark->feature (:content el))])
       :Folder    (collect-features (:content el))
       []))
   elements))

(defn kml->geojson
  "Accepts input stream to KML file and returns GeoJSON as map."
  [is]
  (let [data     (xml/parse is)
        document (or (find-tag (:content data) :Document) data)
        features (collect-features (:content document))]
    (geojson/feature-collection features)))

(defn kmz->geojson
  "Accepts input stream to KMZ file, extracts the embedded KML and returns GeoJSON as map."
  [is]
  (let [zip-is (ZipInputStream. is)]
    (loop [entry (.getNextEntry zip-is)]
      (when entry
        (if (str/ends-with? (.getName entry) ".kml")
          ;; Buffer the entry bytes so xml/parse doesn't interfere with the zip stream
          (let [buf (ByteArrayOutputStream.)
                arr (byte-array 4096)]
            (loop [n (.read zip-is arr)]
              (when (pos? n)
                (.write buf arr 0 n)
                (recur (.read zip-is arr))))
            (kml->geojson (ByteArrayInputStream. (.toByteArray buf))))
          (recur (.getNextEntry zip-is)))))))
