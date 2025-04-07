(ns clj-geo.osm.dataset)

(defn dataset-at-t [histset timestamp]
  (let [extract-fn (fn [element-map versions]
                     (if-let [element (reduce
                                       (fn [final version]
                                         (if (> (:timestamp version) timestamp )
                                           (reduced final)
                                           version))
                                       nil
                                       versions)]
                       (assoc element-map (:id element) element)
                       element-map))]
    {
     :nodes (reduce extract-fn {} (:nodes histset))
     :ways (reduce extract-fn {} (:ways histset))
     :relations (reduce extract-fn {} (:relations histset))}))
