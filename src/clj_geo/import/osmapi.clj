(ns clj-geo.import.osmapi
  "Set of helper fns to work with OSM API."
  (:use
   clj-common.clojure)
  (:require
   [clojure.data.xml :as xml]
   [clj-common.as :as as]
   [clj-common.context :as context]
   [clj-common.localfs :as fs]
   [clj-common.http :as http]
   [clj-common.io :as io]
   [clj-common.json :as json]
   [clj-common.jvm :as jvm]
   [clj-common.logging :as logging]
   [clj-common.path :as path]
   [clj-common.edn :as edn]
   [clj-common.pipeline :as pipeline]
   [clj-common.time :as time]))


(def ^:dynamic *server* "https://api.openstreetmap.org")
(def ^:dynamic *user* (jvm/environment-variable "OSM_USER"))
(def ^:dynamic *password* (jvm/environment-variable "OSM_PASSWORD"))

(def changelog-path ["tmp" "osmapi-changelog"])

(def ^:dynamic *changelog-report*
  (fn [type id comment change-seq]
    (with-open [os (fs/output-stream-by-appending changelog-path)]
      (doseq [change change-seq]
        (edn/write-object
         os
         (assoc
          change
          :timestamp (time/timestamp)
          :type type
          :id id
          :comment comment))
        (io/write-new-line os)))))

;; conversion utils

(def time-formatter (new java.text.SimpleDateFormat "yyyy-MM-dd'T'HH:mm:ss'Z'"))
(defn time-string->timestamp [time-string]
  (.getTime (.parse time-formatter time-string)))
(defn timestamp->time-string [timestamp]
  (.format time-formatter (new java.util.Date timestamp)))

#_(.getTime (.parse time-formatter "2021-05-10T12:12:50Z")) ;; 1620641570000
#_(.format time-formatter (new java.util.Date 1620641570000)) ;; "2021-05-10T12:12:50Z"

(defn node-xml->node
  [node]
  {
   :id (as/as-long (:id (:attrs node)))
   :type :node
   :user (:user (:attrs node))
   :version (as/as-long (:version (:attrs node)))
   :changeset (as/as-long (:changeset (:attrs node)))
   :timestamp (time-string->timestamp (:timestamp (:attrs node)))
   ;; keep longitude and latitude as strings to prevent diff as
   ;; result of conversion ?
   :longitude (:lon (:attrs node))
   :latitude (:lat (:attrs node))
   :tags (reduce
          (fn [tags tag]
            (assoc
             tags
             (:k (:attrs tag))
             (:v (:attrs tag))))
          {}
          (:content node))})

(defn node->node-xml
  [node]
  (xml/element
   :node
   {
    :id (str (:id node))
    :version (str (:version node))
    :changeset (str (:changeset node))
    :timestamp (timestamp->time-string (:timestamp node))
    :lon (:longitude node)
    :lat (:latitude node)}
   (map
    (fn [[key value]]
      ;; to prevent tag names going as keyword, happens during ser / deser
      (xml/element :tag {:k (name key) :v value}))
    (:tags node))))

(defn way-xml->way
  [way-xml]
  (let [way {
             :id (as/as-long (:id (:attrs way-xml)))
             :type :way
             :user (:user (:attrs way-xml))
             :version (as/as-long (:version (:attrs way-xml)))
             :changeset (as/as-long (:changeset (:attrs way-xml)))
             :timestamp (time-string->timestamp (:timestamp (:attrs way-xml)))
             :tags (reduce
                    (fn [tags tag]
                      (assoc
                       tags
                       (:k (:attrs tag))
                       (:v (:attrs tag))))
                    {}
                    (filter
                     #(= (:tag %) :tag)
                     (:content way-xml)))
             :nodes (map
                     #(as/as-long (:ref (:attrs %)))
                     (filter
                      #(= (:tag %) :nd)
                      (:content way-xml)))}]
    ;; support for overpass center extraction, will be added as longitude latitude
    (if-let [center (first (filter #(= (:tag %) :center) (:content way-xml)))]
      (assoc
       way
       :longitude
       (as/as-double (:lon (:attrs center)))
       :latitude
       (as/as-double (:lat (:attrs center))))
      way)))

(defn way->way-xml
  [way]
  (xml/element
   :way
   {
    :id (str (:id way))
    :version (str (:version way))
    :changeset (str (:changeset way))
    :timestamp (timestamp->time-string (:timestamp way))}
   (concat
    (map
     (fn [id]
       (xml/element
        :nd
        {:ref (str id)}))
     (:nodes way))
    (map
     (fn [[key value]]
       ;; to prevent tag names going as keyword, happens during ser / deser
       (xml/element :tag {:k (name key) :v value}))
     (:tags way)))))

(defn relation-xml->relation
  [relation]
  {
   :id (as/as-long (:id (:attrs relation)))
   :type :relation
   :user (:user (:attrs relation))
   :version (as/as-long (:version (:attrs relation)))
   :changeset (as/as-long (:changeset (:attrs relation)))
   :timestamp (time-string->timestamp (:timestamp (:attrs relation)))
   :tags (reduce
          (fn [tags tag]
            (assoc
             tags
             (:k (:attrs tag))
             (:v (:attrs tag))))
          {}
          (filter
           #(= (:tag %) :tag)
           (:content relation)))
   :members (map
             (fn [member]
               {
                ;; todo, why?
                :id (as/as-long (:ref (:attrs member)))
                :type (keyword (:type (:attrs member)))
                :role (let [role (:role (:attrs member))]
                        (if (not (empty? role)) role nil))})
             (filter
              #(= (:tag %) :member)
              (:content relation)))})

(defn relation->relation-xml
  [relation]
  (xml/element
   :relation
   {
    :id (str (:id relation))
    :version (str (:version relation))
    :changeset (str (:changeset relation))
    :timestamp (timestamp->time-string (:timestamp relation))}
   (concat
    (map
     (fn [member]
       (xml/element
        :member
        {
         :ref (str (:id member))
         :role (or (:role member) "")
         :type (name (:type member))}))
     (:members relation))
    (map
     (fn [[key value]]
       ;; to prevent tag names going as keyword, happens during ser / deser
       (xml/element :tag {:k (name key) :v value}))
     (:tags relation)))))

(defn osmc-xml->changeset  
  [osmc-xml]
  (let [modify-seq (mapcat
                    :content
                    (filter
                     #(= (:tag %) :modify)
                     (:content osmc-xml)))
        create-seq (mapcat
                    :content
                    (filter
                     #(= (:tag %) :create)
                     (:content osmc-xml)))
        delete-seq (mapcat
                    :content
                    (filter
                     #(= (:tag %) :delete)
                     (:content osmc-xml)))
        transform-fn (fn [element]
                       (cond
                         (= (:tag element) :node)
                         (node-xml->node element)
                         
                         (= (:tag element) :way)
                         (way-xml->way element)
                         
                         (= (:tag element) :relation)
                         (relation-xml->relation element)
                         
                         :else
                         (throw (ex-info "unknown element" element))))]
    
    {
     :modify (map transform-fn modify-seq)
     :create (map transform-fn create-seq)
     :delete (map transform-fn delete-seq)}))

(def active-changeset-map (atom {}))

#_(close-all-changesets)
#_(deref active-changeset-map)
#_(println (first (first (deref active-changeset-map))))

(defn close-changeset [comment]
  ;; todo call close
  (swap!
   active-changeset-map
   dissoc
   comment))

(defn close-all-changesets []
  (swap!
   active-changeset-map
   (constantly {})))

;; set / get
(defn active-changeset 
  ([comment]
   (get (deref active-changeset-map) comment))
  ([comment changeset]
   (swap! active-changeset-map assoc comment changeset)))

(defn permissions
  "Performs /api/0.6/permissions"
  []
  (xml/parse
   (http/with-basic-auth
     *user*
     *password*
     (http/get-as-stream
      (str *server* "/api/0.6/permissions")))))

(defn changeset-create
  "Performs /api/0.6/changeset/create"
  [comment tags]
  (as/as-long
   (io/input-stream->string
    (http/with-basic-auth
      *user*
      *password*
      (http/put-as-stream
       (str *server* "/api/0.6/changeset/create")
       (io/string->input-stream
        (xml/emit-str
         (xml/element
          :osm
          {}
          (xml/element
           :changeset
           {}
           (conj
            (map
             (fn [[tag value]]
               (xml/element
                :tag
                {:k tag :v value}))
             tags)
            (xml/element
             :tag
             {:k "comment" :v comment})))))))))))

(defn changeset-close
  "Performs /api/0.6/changeset/#id/close"
  [changeset]
  (io/input-stream->string
   (http/with-basic-auth
     *user*
     *password*
     (http/put-as-stream
      (str *server* "/api/0.6/changeset/" changeset "/close")
      (io/string->input-stream
       (xml/emit-str
        (xml/element
         :osm
         {})))))))

(defn changeset-download
  "Performs /api/0.6/changeset/#id/download"
  [changeset]
  (osmc-xml->changeset
   (xml/parse
    (http/with-basic-auth
      *user*
      *password*
      (http/get-as-stream
       (str *server* "/api/0.6/changeset/" changeset "/download"))))))

(defn ensure-changeset
  "Either retrieves active changeset for comment or creates new one"
  [comment tags]
  (if-let [changeset (active-changeset comment)]
    changeset
    (let [changeset (changeset-create comment tags)]
      (active-changeset comment changeset)
      changeset)))

(defn create-relation
  [id version tags members]
  {
   :id id
   :version version
   :tags tags
   :members members})

(defn create-relation-member [type id role]
  {
   :type type
   :id id
   :role role})

;; except node, way and relation objects full methods ( for way and
;; relation ) return dataset object, map of nodes, ways and relations
;; by id

(defn full-xml->dataset
  [elements]
  (reduce
   (fn [dataset element]
     (cond
       (= (:tag element) :node)
       (let [node (node-xml->node element)]
         (update-in dataset [:nodes (:id node)] (constantly node)))
       (= (:tag element) :way)
       (let [way (way-xml->way element)]
         (update-in dataset [:ways (:id way)] (constantly way)))
       (= (:tag element) :relation)
       (let [relation (relation-xml->relation element)]
         (update-in dataset [:relations (:id relation)] (constantly relation)))
       :else
       dataset))
   {}
   elements))

(defn full-xml->histset
  "Creates history dataset (histset) which could be used for historical data
  observation. Similar to dataset but each entry (node, way, relation)  has
  seq of objects sorted by version"
  [elements]
  (reduce
   (fn [dataset element]
     (cond
       (= (:tag element) :node)
       (let [node (node-xml->node element)]
         (update-in
          dataset
          [:nodes (:id node)]
          (fn [versions]
            (sort-by :version (conj versions node)))))
       (= (:tag element) :way)
       (let [way (way-xml->way element)]
         (update-in
          dataset
          [:ways (:id way)]
          (fn [versions]
            (sort-by :version (conj versions way)))))
       (= (:tag element) :relation)
       (let [relation (relation-xml->relation element)]
         (update-in
          dataset
          [:relations (:id relation)]
          (fn [versions]
            (sort-by :version (conj versions relation)))))
       :else
       dataset))
   {}
   elements))

;; util functions

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
        :nodes
        (update-fn (or (:nodes final) {}) (vals (:nodes histset)))
        :ways
        (update-fn (or (:ways final) {}) (vals (:ways histset)))
        :relations
        (update-fn (or (:relations final) {}) (vals (:relations histset)))})
     (first histset-seq)
     (rest histset-seq))))

(defn histset-append-relation
  [histset relation]
  (update-in
   histset
   [:relations (:id relation)]
   #(conj (or % []) relation)))


(defn histset-append-way
  [histset way]
  (update-in
   histset
   [:ways (:id way)]
   #(conj (or % []) way)))


(defn histset-append-node
  [histset node]
  (update-in
   histset
   [:nodes (:id node)]
   #(conj (or % []) node)))


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

#_(merge-histsets
 {
  :nodes
  {
   1 [{:id 1 :version 1} {:id 1 :version 2} {:id 1 :version 3}]}}
 {
  :nodes
  {
   1 [{:id 1 :version 1} {:id 1 :version 2} {:id 1 :version 3} {:id 1 :version 4}]}})
#_{
 :nodes
 {1 [{:id 1, :version 1} {:id 1, :version 2} {:id 1, :version 3} {:id 1, :version 4}]},
 :ways {},
 :relations {}}

(defn node
  "Performs /api/0.6/[node|way|relation]/#id"
  [id]
  (println "[osmapi] node" id)
  (let [node (xml/parse
              (http/get-as-stream
               (str *server* "/api/0.6/node/" id)))]
    (node-xml->node (first (:content node)))))

(defn node-full
  "Wraps result of node in dataset"
  [id]
  (println "[osmapi] node" id)
  {
   :nodes
   {
    id (node id)}})

(defn nodes
  "Performs /api/0.6/[nodes|ways|relations]?#parameters
  Returns dataset object"
  [node-id-seq]
  (println "[OSMAPI /nodes]" (clojure.string/join "," node-id-seq))
  (let [nodes (xml/parse
               (http/get-as-stream
                (str
                 *server*
                 "/api/0.6/nodes?nodes="
                 (clojure.string/join "," node-id-seq))))]
    (full-xml->dataset (:content nodes))))

#_(nodes [5360954914 7579653984])

(defn node-create
  "Performs /api/0.6/[node|way|relation]/create
  Note: changeset should be open changeset
  Note: node should be in same format as returned by node fn, or use new-node"
  [changeset longitude latitude tag-map]
  (let [content (xml/emit-str
                 (xml/element
                  :osm
                  {}
                  (update-in
                   (node->node-xml
                    {
                     :id -1
                     :version 1
                     :longitude longitude
                     :latitude latitude
                     :tags tag-map})
                   [:attrs :changeset]
                   (constantly changeset))))]
    (if-let [is (http/with-basic-auth *user* *password*
                  (http/put-as-stream
                   (str *server* "/api/0.6/node/create")  
                   (io/string->input-stream content)))]
      (let [id (io/input-stream->string is)]
        (*changelog-report*
         :node
         id
         ;; todo
         "node-create"
         (map
          (fn [[tag value]]
            {
             :change :tag-add
             :tag tag
             :value value})
          tag-map))
        id)
      (do
        (logging/report
         {
          :fn clj-geo.import.osmapi/node-create
          :content content})
        nil))))

(defn node-update
  "Performs /api/0.6/[node|way|relation]/#id
  Note: changeset should be open changeset
  Note: node should be in same format as returned by node fn"
  [changeset node]
  (let [id (:id node)
        content (xml/emit-str
                 (xml/element
                  :osm
                  {}
                  (update-in
                   (node->node-xml node)
                   [:attrs :changeset]
                   (constantly changeset))))]
    (if-let [is (http/with-basic-auth *user* *password*
                   (http/put-as-stream
                    (str *server* "/api/0.6/node/" id)
                    (io/string->input-stream content)))]
      (io/input-stream->string is)
      (do
        (logging/report
         {
          :fn clj-geo.import.osmapi/node-update
          :content content})
        nil))))

(defn node-prepare-change-seq
  [node change-seq]
  (reduce
   (fn [node change]
     (cond
       (= :tag-add (:change change))
       (let [{tag :tag value :value} change]
         (if (not (contains? (:tags node)tag))
           (update-in node [:tags] assoc tag value)
           node))
       (= :tag-change (:change change))
       (let [{tag :tag value :new-value} change]
         (update-in node [:tags] assoc tag value ))
       (= :tag-remove (:change change))
       (let [{tag :tag} change]
         (update-in node [:tags] dissoc tag))
       :else
       node))
   node
   change-seq))

(defn node-apply-change-seq
  "Applies given change seq to node, support for osmeditor.
  Retrives node from api, applies changes, creates changeset,
  updated node, closes changeset."
  [id comment change-seq]
  (let [original (node id)
        updated (node-prepare-change-seq original change-seq)]
    (when (not (= original updated))
      (do
        (let [changeset (ensure-changeset comment {})]
          (println "changeset" changeset)
          (node-update changeset updated)
          ;; there is change of reporting change that was already been made
          (*changelog-report* :node id comment change-seq)
          (active-changeset comment changeset)
          #_(changeset-close changeset)
          changeset)))))

(defn node-history
  "Performs /api/0.6/[node|way|relation]/#id/history"
  [id]
  (println (str "[osmapi] /node/" id "/history"))
  (full-xml->histset
   (:content
    (xml/parse
     (http/get-as-stream
      (str *server* "/api/0.6/node/" id "/history"))))))

#_(run!
 #(println (:tags %))
 (get-in
     (node-history 2496289175)
     [:nodes 2496289175]))

(defn way
  "Performs /api/0.6/[node|way|relation]/#id"
  [id]
  (let [way (xml/parse
             (http/get-as-stream
              (str *server* "/api/0.6/way/" id)))]
    (way-xml->way (first (:content way)))))

(defn ways
  "Performs /api/0.6/[nodes|ways|relations]?#parameters
  Returns dataset object"
  [way-id-seq]
  (let [ways (xml/parse
              (http/get-as-stream
               (str
                *server*
                "/api/0.6/ways?ways="
                (clojure.string/join "," way-id-seq))))]
    (full-xml->dataset (:content ways))))

(defn way-full
  "Performs /api/0.6/[way|relation]/#id/full"
  [id]
  (println "[osmapi] way-full" id)
  (let [way (xml/parse
             (http/get-as-stream
              (str *server* "/api/0.6/way/" id "/full")))]
    (full-xml->dataset (:content way))))

#_(def a (way-full 373368159))

(defn way-update
  "Performs /api/0.6/[node|way|relation]/#id
  Note: changeset should be open changeset
  Note: way should be in same format as returned by way fn"
  [changeset way]
  (let [id (:id way)
        content (xml/emit-str
                 (xml/element
                  :osm
                  {}
                  (update-in
                   (way->way-xml way)
                   [:attrs :changeset]
                   (constantly changeset))))]
    (if-let [is (http/with-basic-auth *user* *password*
                  (http/put-as-stream
                   (str *server* "/api/0.6/way/" id)
                   (io/string->input-stream content)))]
      (io/input-stream->string is)
      (do
        (logging/report
         {
          :fn clj-geo.import.osmapi/way-update
          :content content})
        nil))))

#_(clojure.data.xml/emit-str (clojure.data.xml/element :osm {:k "<"} "test"))

(defn way-prepare-change-seq
  [way change-seq]
  (reduce
   (fn [way change]
     (cond
       (= :tag-add (:change change))
       (let [{tag :tag value :value} change]
         (if (not (contains? (:tags way) tag))
           (update-in way [:tags] assoc tag value)
           way))
       (= :tag-change (:change change))
       (let [{tag :tag value :new-value} change]
         (update-in way [:tags] assoc tag value ))
       (= :tag-remove (:change change))
       (let [{tag :tag} change]
         (update-in way [:tags] dissoc tag))
       :else
       way))
   way
   change-seq))

(defn way-apply-change-seq
  "Applies given change seq to way, support for osmeditor.
  Retrives way from api, applies changes, creates changeset,
  updates way, closes changeset."
  [id comment change-seq]
  (let [original (way id)
        updated (way-prepare-change-seq original change-seq)]
    (when (not (= original updated))
      (do
        (println "commiting")
        (let [changeset (ensure-changeset comment {})]
          (println "changeset" changeset)
          (way-update changeset updated)
          ;; there is chance of reporting change that was already been made
          (*changelog-report* :way id comment change-seq)
          (active-changeset comment changeset)
          #_(changeset-close changeset)
          changeset)))))

(defn way-history
  "Performs
  /api/0.6/[node|way|relation]/#id/history"
  [id]
  (println (str "[osmapi] /way/" id "/history"))
  (full-xml->histset
   (:content
    (xml/parse
     (http/get-as-stream
      (str *server* "/api/0.6/way/" id "/history"))))))

#_(run!
 #(println (:tags %))
 (get-in
  (way-history 484429139)
  [:ways 484429139]))

(defn relation
  "Performs /api/0.6/[node|way|relation]/#id"
  [id]
  (let [relation (xml/parse
                  (http/get-as-stream
                   (str *server* "/api/0.6/relation/" id)))]
    (relation-xml->relation (first (:content relation)))))

#_(def a (:content
          (xml/parse
           (http/get-as-stream
            (str *server* "/api/0.6/relation/" 10948917)))))
#_(def b (relation-xml->relation (first a)))
#_(def c (relation->relation-xml b))
#_(def d (relation 10948917))

(defn relation-full
  "Performs /api/0.6/[node|way|relation]/#id/full"
  [id]
  (println "[osmapi] relation-full" id)
  (let [relation (xml/parse
                  (http/get-as-stream
                   (str *server* "/api/0.6/relation/" id "/full")))]
    ;; todo parse, returns raw response
    (full-xml->dataset (:content relation))))

(defn relation-version
  "Performs /api/0.6/[node|way|relation]/#id/#version"
  [id version]
  (let [relation (xml/parse
                  (http/get-as-stream
                   (str *server* "/api/0.6/relation/" id "/" version)))]
    (relation-xml->relation (first (:content relation)))))

#_(relation-version 11043232 1)

(defn relation-update
  "Performs /api/0.6/[node|way|relation]/#id
  Note: changeset should be open changeset
  Note: relation should be in same format as returned by way fn"
  [changeset relation]
  (let [id (:id relation)
        content (xml/emit-str
                 (xml/element
                  :osm
                  {}
                  (update-in
                   (relation->relation-xml relation)
                   [:attrs :changeset]
                   (constantly changeset))))]
    (if-let [is (http/with-basic-auth *user* *password*
                  (http/put-as-stream
                   (str *server* "/api/0.6/relation/" id)
                   (io/string->input-stream content)))]
      (io/input-stream->string is)
      (do
        (logging/report
         {
          :fn clj-geo.import.osmapi/relation-update
          :content content})
        nil))))

#_(def a (relation-version 11164146 2))

(defn relation-prepare-change-seq
  [relation change-seq]
  (reduce
   (fn [relation change]
     (cond
       (= :tag-add (:change change))
       (let [{tag :tag value :value} change]
         (if (not (contains? (:tags relation) tag))
           (update-in relation [:tags] assoc tag value)
           relation))
       (= :tag-change (:change change))
       (let [{tag :tag value :new-value} change]
         (update-in relation [:tags] assoc tag value ))
       (= :tag-remove (:change change))
       (let [{tag :tag} change]
         (update-in relation [:tags] dissoc tag))
       :else
       relation))
   relation
   change-seq))

(defn relation-apply-change-seq
  "Applies given change seq to relation, support for osmeditor.
  Retrives relation from api, applies changes, creates changeset,
  updates relation, closes changeset."
  [id comment change-seq]
  (let [original (relation id)
        updated (relation-prepare-change-seq original change-seq)]
    (when (not (= original updated))
      (do
        (println "commiting")
        (let [changeset (ensure-changeset comment {})]
          (println "changeset" changeset)
          (relation-update changeset updated)
          ;; there is chance of reporting change that was already been made
          (*changelog-report* :relation id comment change-seq)
          (active-changeset comment changeset)
          #_(changeset-close changeset)
          changeset)))))

;; todo check previos
;; todo create relation-update ...
;; todo test
;; todo add support for relation in osmeditor:559

(defn relation-history
  "Performs
  /api/0.6/[node|way|relation]/#id/history"
  [id]
  (println (str "[osmapi] /relation/" id "/history"))
  (full-xml->histset
   (:content
    (xml/parse
     (http/get-as-stream
      (str *server* "/api/0.6/relation/" id "/history"))))))

(defn relation-histset
  "Creates histset containing relation and all ways and nodes required over time"
  [histset id]
  ;; go over relation members, accumulate
  ;; retrieve all ways needed for relation over time
  ;; retrieve all nodes needed for relation over time


  ;; todo
  nil
  )

#_(run!
 #(println "count of members:" (count (:members %)))
 (get-in
  (relation-history 12693206)
  [:relations 12693206]))

#_(def a (relation-history 10903395))
#_(first (:members (first (:elements a))))
#_{:type "way", :ref 373445686, :role ""}

#_(count (:elements a))
#_(def b (:members (get (:elements a) 0)))
#_(def c (:members (get (:elements a) 4)))

(defn calculate-member-change
  "Note: current version doesn't understand order, better would be to identify
  members which are present in both versions and calculate changes between those
  pairs. Concept in paper notes 20200608."
  
  [user timestamp version changeset old-seq new-seq]
  (let [split-on-fn (fn [id coll]
                      (reduce
                       (fn [[before hit after] elem]
                         (if (nil? hit)
                           (if (= (:id elem) id)
                             [before elem after]
                             [(conj before elem) hit  after])
                           [before hit (conj after elem)]))
                       [[] nil []]
                       coll))
        old-seq (map #(assoc % :id (str (str (first (name (:type %)))) (:id %))) old-seq)
        new-seq (map #(assoc % :id (str (str (first (name (:type %)))) (:id %))) new-seq)]
    (if (not (= old-seq new-seq))
      (loop [old-seq old-seq
             new-seq new-seq
             change-seq []]
        (if-let [old (first old-seq)]
          (let [[before-seq new after-seq] (split-on-fn (:id old) new-seq)]
            (if (some? new)
              (let [[step-change-seq rest-old-seq]
                    (reduce
                     (fn [[step-change-seq rest-old-seq] before]
                       (let [[before-seq same after-seq] (split-on-fn (:id before) rest-old-seq)]
                         (if (some? same)
                           [
                            (conj
                             step-change-seq
                             {
                              :change :member-order
                              :user user
                              :timestamp timestamp
                              :version version
                              :changeset changeset
                              :type (:type before)
                              :id (:id before)
                              :role (if (not (empty? (:role before))) (:role before) nil)})
                            (concat before-seq after-seq)]
                           [
                            (conj
                             step-change-seq
                             {
                              :change :member-add
                              :user user
                              :timestamp timestamp
                              :version version
                              :changeset changeset
                              :type (:type before)
                              :id (:id before)
                              :role (if (not (empty? (:role before))) (:role before) nil)})
                            rest-old-seq])))
                     [[] (rest old-seq)]
                     before-seq)]
                (recur
                 rest-old-seq
                 after-seq
                 (concat
                  change-seq
                  step-change-seq)))
              (recur
               (rest old-seq)
               new-seq
               (conj
                change-seq
                {
                 :change :member-remove
                 :user user
                 :timestamp timestamp
                 :version version
                 :changeset changeset
                 :type (:type old)
                 :id (:id old)
                 :role (if (not (empty? (:role old))) (:role old) nil)}))))
          (concat
           change-seq
           (map
            (fn [member]
              {
               :change :member-add
               :user user
               :timestamp timestamp
               :version version
               :changeset changeset
               :type (:type member)
               :id (:id member)
               :role (if (not (empty? (:role member))) (:role member) nil)})
            new-seq))))
      [])))

#_(calculate-member-change
 1 1 1 1
 [{:id 1 :type "way" :ref 1} {:id 3 :type "way" :ref 3} {:id 5 :type "way" :ref 5}]
 [{:id 1 :type "way" :ref 1} {:id 4 :type "way" :ref 4} {:id 3 :type "way" :ref 3} {:id 5 :type "way" :ref 5}])

#_(calculate-member-change
 1 1 1 1
 [{:id 1 :type "way" :ref 1} {:id 2 :type "way" :ref 2}]
 [{:id 2 :type "way" :ref 2} {:id 3 :type "way" :ref 3}])


#_(calculate-member-change
 1 1 1 1
 [{:id 1 :type "way"} {:id 2 :type "way" :ref 2}]
 [{:id 2 :type "way"} {:id 3 :type "way" :ref 3}])
#_(
 {:change :member-remove, :user 1, :timestamp 1, :version 1, :changeset 1, :type "way", :id "w1", :role nil}
 {:change :member-add, :user 1, :timestamp 1, :version 1, :changeset 1, :type "way", :id "w3", :role nil})


#_(calculate-member-change
 1 1 1 1
 [{:id 1 :type "way"} {:id 2 :type "way"}]
 [{:id 2 :type "way"} {:id 1 :type "way"}])
#_(
 {:change :member-order, :user 1, :timestamp 1, :version 1, :changeset 1, :type "way", :id "w2", :role nil})


#_(let [relation-history (relation-history 11043543)]
    (calculate-member-change
     1 1 1 1
     (get-in relation-history [:elements 0 :members])
     (get-in relation-history [:elements 1 :members])))

#_(defn split-on [id coll]
  (reduce
   (fn [[before hit after] elem]
     (if (nil? hit)
       (if (= (:id elem) id)
         [before elem after]
         [(conj before elem) hit  after])
       [before hit (conj after elem)]))
   [[] nil []]
   coll))

#_(split-on 7 [{:id 1} {:id 7} {:id 5} {:id 7}])

(defn print-element
  "Used for debugging of compare-element"
  [element]
  (println "type" (:type element))
  (println "id" (:id element))
  (println "version" (:version element))
  (println (str "coordinates " (:lon element) ", " (:lat element)))
  (println "tags")
  (run!
   #(println (str "\t" (name (first %)) "=" (second %)))
   (:tags element))
  (println "nodes")
  (run!
   #(println (str "\t" %))
   (:nodes element))
  (println "members")
  (run!
   #(println (str "\t" (:type %) " " (:ref %) " as " (:role %)))
   (:members element)))

#_(print-element (last (:elements (way-history 827371796))))

(defn compare-element
  "Note use print-element for debugging"
  [old new]
  (if (nil? old)
    ;; creation
    (concat
     [{
       :change :create
       :user (:user new)
       :timestamp (:timestamp new)
       :version (:version new)
       :changeset (:changeset new)}]
     (map
      (fn [[tag value]]
        {
         :change :tag-add
         :user (:user new)
         :timestamp (:timestamp new)
         :version (:version new)
         :changeset (:changeset new)
         :tag tag
         :value value})
      (:tags new))
     (calculate-member-change
      (:user new) (:timestamp new) (:version new) (:changeset new)
      '() (:members new)))
    (if (= (:visible new) false)
      ;; deletion
      [{
        :change :delete
        :user (:user new)
        :timestamp (:timestamp new)
        :version (:version new)
        :changeset (:changeset new)}]
      ;; both exists
      (let [change-seq (filter
                       some?
                       (concat
                        ;; test location, nodes and members
                        ;; switch depending on type
                        (cond
                          (= (:type new) :node)
                          (when (or
                                 (not (= (:longitude old) (:longitude new)))
                                 (not (= (:latitude old) (:latitude new))))
                            [{
                              :change :location
                              :user (:user new)
                              :timestamp (:timestamp new)
                              :version (:version new)
                              :changeset (:changeset new)
                              :old (select-keys old [:lon :lat])
                              :new (select-keys new [:lon :lat])}])

                          (= (:type new) :way)
                          (when (not (= (:nodes old) (:nodes new)))
                            [{
                              :change :nodes
                              :user (:user new)
                              :timestamp (:timestamp new)
                              :version (:version new)
                              :changeset (:changeset new)
                              :old (:nodes old)
                              :new (:nodes new)}])

                          (= (:type new) :relation)
                          (when (not (= (:members old) (:members new)))
                            (calculate-member-change
                             (:user new) (:timestamp new) (:version new) (:changeset new)
                             (:members old) (:members new))
                            #_[{
                                :change :members
                                :user (:user new)
                                :timestamp (:timestamp new)
                                :version (:version new)
                                :changeset (:changeset new)
                                :members (:members new)}]))
                        ;; test new tags
                        (map
                         (fn [[tag value]]
                           {
                            :change :tag-add
                            :user (:user new)
                            :timestamp (:timestamp new)
                            :version (:version new)
                            :changeset (:changeset new)
                            :tag tag
                            :value value})
                         (filter
                          #(not (contains? (:tags old) (first %)))
                          (:tags new)))
                        ;; test changed tags
                        (map
                         (fn [[tag value]]
                           {
                            :change :tag-change
                            :user (:user new)
                            :timestamp (:timestamp new)
                            :version (:version new)
                            :changeset (:changeset new)
                            :tag tag
                            :new-value value
                            :old-value (get-in old [:tags tag])})
                         (filter
                          #(and
                            (contains? (:tags old) (first %))
                            (not (= (get-in old [:tags (first %)]) (second %))))
                          (:tags new)))
                        ;; test removed tags
                        (map
                         (fn [[tag value]]
                           {
                            :change :tag-remove
                            :user (:user new)
                            :timestamp (:timestamp new)
                            :version (:version new)
                            :changeset (:changeset new)
                            :tag tag
                            :value value})
                         (filter
                          #(not (contains? (:tags new) (first %)))
                          (:tags old)))))]
       (if (> (count change-seq) 0)
         change-seq
         [{
           :change :no-change
           :user (:user new)
           :timestamp (:timestamp new)
           :version (:version new)
           :changeset (:changeset new)}])))))

(defn calculate-node-change 
  "Support two modes, retrieve entire history or just at given version"
  ([id]
   (first
    (reduce
     (fn [[changes previous] next]
       [
        (concat
         changes
         (compare-element previous next))
        next])
     []
     (get-in (node-history id) [:nodes id]))))
  ([id version]
   (let [versions (get-in (node-history id) [:nodes id])]
     (compare-element
      (first
       (filter #(= (:version %) (dec version)) versions))
      (first
       (filter #(= (:version %) version) versions))))))

#_(calculate-node-change 3232657154)
#_(node-history 3232657154)
#_(node-history 1637504812)
#_(calculate-node-change 1637504812)
#_(calculate-node-change 1637504812 3)

(defn calculate-way-change
  "Support two modes, retrieve entire history or just at given version"
  ([id]
   (first
    (reduce
     (fn [[changes previous] next]
       [
        (concat
         changes
         (compare-element previous next))
        next])
     []
     (get-in (way-history id) [:ways id]))))
  ([id version]
   (let [versions (get-in (way-history id) [:ways id])]
     (compare-element
      (first
       (filter #(= (:version %) (dec version)) versions))
      (first
       (filter #(= (:version %) version) versions))))))

#_(keys (relation-history 12452310))
#_(first (:members (first (get-in (relation-history 12452310) [:relations 12452310]))))

(defn calculate-relation-change
  "Support two modes, retrieve entire history or just at given version"
  ([id]
   (first
    (reduce
     (fn [[changes previous] next]
       [
        (concat
         changes
         (compare-element previous next))
        next])
     []
     (get-in (relation-history id) [:relations id]))))
  ([id version]
   (let [versions (get-in (relation-history id) [:relatiosns id])]
     (compare-element
      (first
       (filter #(= (:version %) (dec version)) versions))
      (first
       (filter #(= (:version %) version) versions))))))

#_(run!
   println
   (calculate-relation-change 12452310))

#_(run!
 #(println (clojure.string/join "," (map :id (:members %))))
 (get-in (relation-history 12452310) [:relations 12452310]))

(defn report-change [version change]
  (when (not (= version (:version change)))
    (println
     (str
      "v: " (:version change)
      ", t: " (:timestamp change) 
      ", c: " (:changeset change)
      ", u: " (:user change))))
  (condp = (:change change)
    :create
    (println "\tcreated")
    :location
    (println "\tmoved")
    :nodes
    (println "\tchanged nodes")
    :members
    (println "\tchanged members")
    :tag-add
    (println "\t+" (name (:tag change)) "=" (:value change))
    :tag-remove
    (println "\t-" (name (:tag change)))
    :tag-change
    (println "\t!" (name (:tag change)) (:old-value change) "->" (:new-value change))
    :else
    (println "\tunknown"))
  (:version change))

(defn report-node-history
  [id]
  (println "node history:" id)
  (reduce
   report-change
   nil
   (calculate-node-change id))
  nil)

(defn report-way-history
  [id]
  (println "way history:" id)
  (reduce
   report-change
   nil
   (calculate-way-change id))
  nil)

(defn report-relation-history
  [id]
  (println "relation history:" id)
  (reduce
   report-change
   nil
   (calculate-relation-change id))
  nil)

#_(report-node-history 2911991364)
#_(report-node-history 60571493)
#_(report-way-history 404209416)
#_(report-relation-history 10833727)

(defn map-bounding-box
  "Performs /api/0.6/map"
  [left bottom right top]
  (let [bbox (xml/parse
              (http/get-as-stream
               (str
                *server*
                "/api/0.6/map?bbox=" left "," bottom "," right "," top)))]
    ;; todo parse, returns raw response
    (full-xml->dataset (:content bbox))))

#_(map-bounding-box 20.61906 45.19066 20.62567 45.19471)

(defn gpx-bounding-box
  "Performs /api/0.6/trackpoints"
  [min-longitude max-longitude min-latitude max-latitude]
  (xml/parse
   (http/get-as-stream
    (str
     *server*
     "/api/0.6/trackpoints?bbox="
     min-longitude "," min-latitude "," max-longitude "," max-latitude
     "&page=0"))))

;; util functions to work with extracted dataset
(defn merge-datasets [& dataset-seq]
  (reduce
   (fn [final dataset]
     (assoc
      final
      :nodes
      (merge (:nodes final) (:nodes dataset))
      :ways
      (merge (:ways final) (:ways dataset))
      :relations
      (merge (:relations final) (:relations dataset))))
   (first dataset-seq)
   (rest dataset-seq)))

(defn dataset-append-node [dataset node]
  (update-in dataset [:nodes (:id node)] (constantly node) ))

(defn dataset-append-way [dataset way]
  (update-in dataset [:ways (:id way)] (constantly way) ))

(defn dataset-append-relation [dataset relation]
  (update-in dataset [:relations (:id relation)] (constantly relation) ))

(defn extract-way
  [dataset way-id]
  (update-in
   (get-in dataset [:ways way-id])
   [:nodes]
   (fn [ids]
     (map
      #(get-in dataset [:nodes %])
      ids))))

(defn extract-relation
  [dataset relation-id]
  (update-in
   (get-in dataset [:relations relation-id])
   [:ways]
   (fn [ids]
     (map
      #(extract-way dataset %)))))


;; util functions to work with OsmChange osmc
(defn create-changeset [changeset create-seq modify-seq delete-seq]
  (let [convert-fn (fn [element]
                     (let [element (assoc element :changeset changeset)]
                       (cond
                         (= (:type element) :node)
                         (node->node-xml element)

                         (= (:type element) :way)
                         (way->way-xml element)

                         (= (:type element) :relation)
                         (relation->relation-xml element))))]
    (xml/element
     :osmChange
     {}
     [
      (xml/element
       :create
       {}
       (map convert-fn create-seq))
      (xml/element
       :modify
       {}
       (map convert-fn modify-seq))
      (xml/element
       :delete
       {}
       (map convert-fn delete-seq))])))

(defn note-create
  "Performs POST /api/0.6/notes"
  [longitude latitude text]
  (if-let [is (http/with-basic-auth *user* *password*
                (http/post-as-stream
                 (str *server* "/api/0.6/notes?"
                      "lat=" latitude "&lon=" longitude "&text=" (url-encode text))
                 (io/string->input-stream "")))]
    (as/as-long
     (first
      (:content
       (first
        (filter #(= (:tag %) :id) (:content (first (:content (xml/parse is)))))))))))
