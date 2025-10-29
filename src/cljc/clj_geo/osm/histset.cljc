(ns clj-geo.osm.histset)

(defn histset-create [] {})

(defn histset-merge [& histset-seq]
  (let  [update-fn (fn [element-map element-seq]
                     (println "map" element-map)
                     (reduce
                      (fn [element-map element]
                        (println "element" element)
                        (let [id (:id (first element))]
                          (println id)
                          (if-let [old-element (get element-map id)]
                            ;; element with most versions win
                            (if (> (count old-element) (count element))
                              element-map
                              (assoc element-map id element))
                            (assoc element-map id element))))
                      element-map
                      element-seq))]
    (reduce
     (fn [final histset]
       {
        :node
        (update-fn (or (:node final) {}) (vals (:node histset)))
        :way
        (update-fn (or (:way final) {}) (vals (:way histset)))
        :relation
        (update-fn (or (:relation final) {}) (vals (:relation histset)))})
     (first histset-seq)
     (rest histset-seq))))

(defn histset-append-relation
  [histset relation]
  (update-in
   histset
   [:relation (:id relation)]
   #(conj (or % []) relation)))


(defn histset-append-way
  [histset way]
  (update-in
   histset
   [:way (:id way)]
   #(conj (or % []) way)))


(defn histset-append-node
  [histset node]
  (update-in
   histset
   [:node (:id node)]
   #(conj (or % []) node)))

(defn debug-histset [histset]
  (println "relations:")
  (doseq [[id relation] (:relation histset)]
    (println "\t" id)
    (doseq [version relation]
      (println "\t\t" (:timestamp version) (:changeset version) (:version version) (:user version))))
  (println "ways:")
  (doseq [[id way] (:way histset)]
    (println "\t" id)
    (doseq [version way]
      (println "\t\t" (:timestamp version) (:changeset version) (:version version) (:user version))))
  (println "node:")
  (doseq [[id node] (:node histset)]
    (println "\t" id)
    (doseq [version node]
      (println "\t\t" (:timestamp version) (:changeset version) (:version version) (:user version))))  )

#_(merge-histsets
   {
    :node
    {
     1 [{:id 1 :version 1} {:id 1 :version 2} {:id 1 :version 3}]}}
   {
    :node
    {
     1 [{:id 1 :version 1} {:id 1 :version 2} {:id 1 :version 3} {:id 1 :version 4}]}})
#_{
 :node
 {1 [{:id 1, :version 1} {:id 1, :version 2} {:id 1, :version 3} {:id 1, :version 4}]},
 :way {},
 :relation {}}



