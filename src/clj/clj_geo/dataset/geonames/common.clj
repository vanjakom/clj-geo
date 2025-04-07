(ns clj-geo.dataset.geonames.common)

(defn name->keyword [name]
  (keyword
    (.toLowerCase
      (.replace
        (.trim name)
        " "
        "-"))))

(defn optional-long [value]
  (if-let [value value]
    (Long/parseLong value)))

(defn optional-double [value]
  (if-let [value value]
    (Double/parseDouble value)))

(defn tsv-line->map [header line]
  (let [parts (.split line "\t")]
    (zipmap header parts)))

(defn tsv-line->typed-map [header-type line]
  (let [parts (.split line "\t")]
    (zipmap
      (map first header-type)
      (map
        (fn [type-fn value]
          (type-fn value))
        (map second header-type)
        parts))))

(defn tsv-line->header [line]
  (map
    keyword
    (map
      #(.toLowerCase %)
      (.split line "\t"))))
