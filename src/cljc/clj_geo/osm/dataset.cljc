(ns clj-geo.osm.dataset)

(defn dataset-at-t [histset timestamp]
  (let [extract-fn (fn [element-map [id versions]]
                     (if-let [element (reduce
                                       (fn [final version]
                                         (if (> (:timestamp version) timestamp )
                                           (reduced final)
                                           version))
                                       nil
                                       versions)]
                       (assoc element-map id element)
                       element-map))]
    {
     :node (reduce extract-fn {} (:node histset))
     :way (reduce extract-fn {} (:way histset))
     :relation (reduce extract-fn {} (:relation histset))}))

(defn merge-datasets [& dataset-seq]
  (reduce
   (fn [final dataset]
     (assoc
      final
      :node
      (merge (:node final) (:node dataset))
      :way
      (merge (:way final) (:way dataset))
      :relation
      (merge (:relation final) (:relation dataset))))
   (first dataset-seq)
   (rest dataset-seq)))

(defn dataset-append-node [dataset node]
  (update-in dataset [:node (:id node)] (constantly node) ))

(defn dataset-append-way [dataset way]
  (update-in dataset [:way (:id way)] (constantly way) ))

(defn dataset-append-relation [dataset relation]
  (update-in dataset [:relation (:id relation)] (constantly relation) ))

(defn extract-way
  [dataset way-id]
  (update-in
   (get-in dataset [:way way-id])
   [:nodes]
   (fn [ids]
     (map
      #(get-in dataset [:node %])
      ids))))

(defn extract-relation
  [dataset relation-id]
  (update-in
   (get-in dataset [:relation relation-id])
   [:ways]
   (fn [ids]
     (map
      #(extract-way dataset %)))))

(defn way-center [dataset id]
  (let [way (get-in dataset [:way id])
        min-longitude (apply
                       min
                       (map
                        #(:longitude (get-in dataset [:node %]))
                        (:nodes way)))
        max-longitude (apply
                       max
                       (map
                        #(:longitude (get-in dataset [:node %]))
                        (:nodes way)))
        min-latitude (apply
                      min
                      (map
                       #(:latitude (get-in dataset [:node %]))
                       (:nodes way)))
        max-latitude (apply
                      max
                      (map
                       #(:latitude (get-in dataset [:node %]))
                       (:nodes way)))]
    {
     :longitude (+ min-longitude (/ (- max-longitude min-longitude) 2))
     :latitude (+ min-latitude (/ (- max-latitude min-latitude) 2))}))

#_(way-center valjevske-dataset 641859168)

(defn relation-center [dataset id]
  (let [relation (get-in dataset [:relation id])
        nodes (mapcat
               (fn [member]
                 (let [way (get-in dataset [:way (:id member)])]
                   (map
                    #(get-in dataset [:node %])
                    (:nodes way))))
               (filter #(= (:type %) :way) (:members relation)))
        min-longitude (apply min (map #(:longitude %) nodes))
        max-longitude (apply max (map  #(:longitude %) nodes))
        min-latitude (apply min (map  #(:latitude %) nodes))
        max-latitude (apply max (map  #(:latitude %) nodes))]
    {
     :longitude (+ min-longitude (/ (- max-longitude min-longitude) 2))
     :latitude (+ min-latitude (/ (- max-latitude min-latitude) 2))}))

#_(relation-center valjevske-dataset 11835344)
