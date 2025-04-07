(ns clj-geo.import.osm
  (:use
   clj-common.clojure)
  (:require
   [clojure.data.xml :as xml]
   [clojure.core.async :as async]   
   [clj-common.as :as as]
   [clj-common.context :as context]
   [clj-common.logging :as logging]
   [clj-common.io :as io]
   [clj-common.http :as http]
   [clj-common.cache :as cache]
   [clj-common.localfs :as fs]
   [clj-common.view :as view]
   [clj-common.context :as context]
   [clj-geo.env :as env])
  (:import
   java.io.InputStream))

(defn parse-xml-entry [entry]
  (cond
    (= (:tag entry) :node)
    (let [longitude (as/as-double (:lon (:attrs entry)))
          latitude (as/as-double (:lat (:attrs entry)))
          id (as/as-long (:id (:attrs entry)))
          content (map parse-xml-entry (:content entry))
          tags (reduce
                (fn [tags {key :key value :value}]
                  (assoc
                   tags
                   key
                   value))
                {}
                (filter #(= (:type %) :tag) content))]
      {
       :type :node
       :id id
       :longitude longitude
       :latitude latitude
       :tags tags})

    (= (:tag entry) :tag)
    {
     :type :tag
     :key (:k (:attrs entry))
     :value (:v (:attrs entry))}
    
    (= (:tag entry) :way)
    (let [id (as/as-long (:id (:attrs entry)))
          content (map parse-xml-entry (:content entry))
          nodes (filter #(= (:type %) :node-ref) content)
          tags (reduce
                (fn [tags {key :key value :value}]
                  (assoc
                   tags
                   key
                   value))
                {}
                (filter #(= (:type %) :tag) content))]
      {
       :type :way
       :id id
       :nodes nodes
       :tags tags})
    
    (= (:tag entry) :nd)
    (let [ref (as/as-long (:ref (:attrs entry)))]
      {
       :type :node-ref
       :id ref})

    (= (:tag entry) :relation)
    (let [id (as/as-long (:id (:attrs entry)))
          content (map parse-xml-entry (:content entry))
          members (filter #(= (:type %) :member) content)
          tags (reduce
                (fn [tags {key :key value :value}]
                  (assoc
                   tags
                   key
                   value))
                {}
                (filter #(= (:type %) :tag) content))]
      {
       :type :relation
       :id id
       :members members
       :tags tags})
    
    (= (:tag entry) :member)
    (let [type (:type (:attrs entry))
          ref (as/as-long (:ref (:attrs entry)))
          role (:role (:attrs entry))]
      {
       :type :member
       :ref-type type
       :ref ref
       :role (if (> (count role) 0) role nil)})
    
    :default nil))

(defn read-osm
  "Reads fully in memory OSM file."
  [input-stream]
  (todo-warn "support relations")
  (let [content (:content (xml/parse input-stream))
        [bounds nodes ways] (reduce
                             (fn [[bounds nodes ways] entry]
                               (let [element (parse-xml-entry entry)]
                                 (cond
                                   (= (:type element) :node) [bounds (conj nodes element) ways]
                                   (= (:type element) :way) [bounds nodes (conj ways element)]
                                   :default [bounds nodes ways])))
                             [nil '() '()]
                             content)]
    {
     :nodes nodes
     :ways ways}))

(defn stream-osm
  "Streams parsed osm data. To be used for fast filtering."
  [input-stream]
  (map
   parse-xml-entry
   (:content (xml/parse input-stream))))

(defn merge-nodes-into-way
  ([node-seq way-seq]
   (let [node-map (view/seq->map :id node-seq)]
     (map
      (fn [way]
        (assoc
         way
         :nodes
         (map
          #(get node-map (:id %))
          (:nodes way))))
      way-seq)))
  ([osm]
   (merge-nodes-into-way (:nodes osm) (:ways osm))))

(defn read-osm-go
  "Reads and performs parsing of OSM export file, emitting entries to given channel"
  [context path ch]
  (async/go
    (context/set-state context "init")
    (try
      (with-open [input-stream (fs/input-stream path)]
        (loop [entries (filter some? (stream-osm input-stream))]
          (when-let [entry (first entries)]
            (context/set-state context "step")
            (when (async/>! ch entry)
              (context/counter context "read")
              (recur (rest entries))))))
      (catch Exception e (context/error context e {:fn read-osm-go :path path}))
      (finally
        (async/close! ch)
        (context/set-state context "completion")))))

(defn read-osm-pbf-go
  "Uses osm4j to read pbf, should support historical pbfs"
  [context path node-ch way-ch relation-ch]
  (async/thread
    (context/set-state context "init")
    (with-open [is (fs/input-stream path)]
      (let [iterator (new  de.topobyte.osm4j.pbf.seq.PbfIterator is true)]
        (while (.hasNext iterator)
          (let [next (.next iterator)
                entity (.getEntity next)
                id (.getId entity)
                tags (into {} (map (fn [index]
                                     (let [tag (.getTag entity index)]
                                       [(.getKey tag) (.getValue tag)]))
                                   (range (.getNumberOfTags entity))))
                metadata (.getMetadata entity)
                user (when (some? metadata) (.getUser metadata))
                timestamp (when (some? metadata) (.getTimestamp metadata))
                visible (when (some? metadata) (.isVisible metadata))
                object {
                          :id id
                          ;; removed, legacy, to reduce footprint
                          ;; :osm tags
                          :tags tags
                          :user user
                          :timestamp timestamp
                        :visible visible}]
            (def a entity)
            (cond
              (= (.getType next) de.topobyte.osm4j.core.model.iface.EntityType/Node)
              (let [node (assoc object
                                :type :node
                                :longitude (when visible (.getLongitude entity))
                                :latitude (when visible (.getLatitude entity)))]
                (if node-ch
                  (do
                    (async/>!! node-ch node)
                    (context/counter context "node-out"))
                  (context/counter context "node-skip")))

              (= (.getType next) de.topobyte.osm4j.core.model.iface.EntityType/Way)
              (let [way (assoc object
                               :type :way
                               :nodes (map (fn [index]
                                             (.getNodeId entity index))
                                           (range (.getNumberOfNodes entity))))]
                (if way-ch
                  (do
                    (async/>!! way-ch way)
                    (context/counter context "way-out"))
                  (context/counter context "way-skip")))

              (= (.getType next) de.topobyte.osm4j.core.model.iface.EntityType/Relation)
              (let [relation (assoc object
                                    :type :relation
                                    :members
                                    (map
                                     (fn [index]
                                       (let [member (.getMember entity index)]
                                         {
                                          :type (cond
                                                  (= (.getType member)
                                                     de.topobyte.osm4j.core.model.iface.EntityType/Node)
                                                 :node
                                                 (= (.getType member)
                                                    de.topobyte.osm4j.core.model.iface.EntityType/Way)
                                                 :way
                                                 (= (.getType member)
                                                    de.topobyte.osm4j.core.model.iface.EntityType/Relation)
                                                 :relation

                                                 :else
                                                 nil)
                                          :id (.getId member)
                                          :role (.getRole member)}))
                                     (range (.getNumberOfMembers entity))))]
                (if relation-ch
                  (do
                    (async/>!! relation-ch relation)
                    (context/counter context "relation-out"))
                  (context/counter context "relation-skip")))

              :else
              (context/counter context "unknown-type"))))))
    (when node-ch
      (async/close! node-ch))
    (when way-ch
      (async/close! way-ch))
    (when relation-ch
      (async/close! relation-ch))
    (context/set-state context "completion")))

(defn extract-recursive-from-split
  "Takes node-set, way-set, relation-set and input streams of nodes, ways and relations.
  Reads first relations, writes to out ones needed, accumulates ways, then reads ways,
  writes needed to output and accumulates nodes. Finally reads nodes and writes
  to out needed."
  [context node-set way-set relation-set
   node-in way-in relation-in
   node-out way-out relation-out]
  (async/go
    (context/set-state context "init")
    (loop [node-set node-set
           way-set way-set
           relation (async/<! relation-in)]
      (context/set-state context "relation-scan")
      (if relation
        (if (contains? relation-set (:id relation))
          (do
            (async/>! relation-out relation)
            (context/increment-counter context "relation-out")
            (recur
             (into node-set (filter some? (map
                                           (fn [member]
                                             (when (= (:type member) :node)
                                               (:id member)))
                                           (:members relation))))
             (into way-set (filter some? (map
                                          (fn [member]
                                            (when (= (:type member) :way)
                                              (:id member)))
                                          (:members relation))))
             (async/<! relation-in)))
          (recur
           node-set
           way-set
           (async/<! relation-in)))
        (loop [node-set node-set
               way (async/<! way-in)]
          (context/set-state context "way-scan")
          (if way
            (if (contains? way-set (:id way))
              (do
                (async/>! way-out way)
                (context/increment-counter context "way-out")
                (recur
                 (into node-set (:nodes way))
                 (async/<! way-in)))
              (recur
               node-set
               (async/<! way-in)))
            (loop [node (async/<! node-in)]
              (context/set-state context "node-scan")
              (if node
                (if (contains? node-set (:id node))
                  (do
                    (context/increment-counter context "node-out")
                    (async/>! node-out node)
                    (recur (async/<! node-in)))
                  (recur (async/<! node-in)))
                (do
                  (async/close! node-out)
                  (async/close! way-out)
                  (async/close! relation-out)
                  (context/set-state context "completion"))))))))))

(defn extract-recursive-by-fn-from-split
  "Takes node-set, way-set, relation-set and input streams of nodes, ways and relations.
  Reads first relations, writes to out ones needed, accumulates ways, then reads ways,
  writes needed to output and accumulates nodes. Finally reads nodes and writes
  to out needed."
  [context node-fn way-fn relation-fn
   node-in way-in relation-in
   node-out way-out relation-out]
  (let [node-fn (or node-fn (constantly false))
        way-fn (or way-fn (constantly false))
        relation-fn (or relation-fn (constantly false))]
    (async/go
      (context/set-state context "init")
      (loop [node-set #{}
             way-set #{}
             relation (async/<! relation-in)]
        (context/set-state context "relation-scan")
        (if relation
          (if (relation-fn relation)
            (do
              (async/>! relation-out relation)
              (context/increment-counter context "relation-out")
              (recur
               (into node-set (filter some? (map
                                             (fn [member]
                                               (when (= (:type member) :node)
                                                 (:id member)))
                                             (:members relation))))
               (into way-set (filter some? (map
                                            (fn [member]
                                              (when (= (:type member) :way)
                                                (:id member)))
                                            (:members relation))))
               (async/<! relation-in)))
            (recur
             node-set
             way-set
             (async/<! relation-in)))
          (loop [node-set node-set
                 way (async/<! way-in)]
            (context/set-state context "way-scan")
            (if way
              (if (or
                   (way-fn way)
                   (contains? way-set (:id way)))
                (do
                  (async/>! way-out way)
                  (context/increment-counter context "way-out")
                  (recur
                   (into node-set (:nodes way))
                   (async/<! way-in)))
                (recur
                 node-set
                 (async/<! way-in)))
              (loop [node (async/<! node-in)]
                (context/set-state context "node-scan")
                (if node
                  (if (or
                       (node-fn node)
                       (contains? node-set (:id node)))
                    (do
                      (context/increment-counter context "node-out")
                      (async/>! node-out node)
                      (recur (async/<! node-in)))
                    (recur (async/<! node-in)))
                  (do
                    (async/close! node-out)
                    (async/close! way-out)
                    (async/close! relation-out)
                    (context/set-state context "completion")))))))))))

(defn check-connected?
  "For given relation checks that all way members are connected, useful for hiking
  relation checks. Returns vector of connected ways and boolean"
  [way-map relation]
  (println "relation: " (:id relation))
  (loop [end-set nil
         ;; todo quick fix for json serialized / deserialized data
         ways (map :id (filter #(= (name (:type %)) "way") (:members relation)))
         connected-way-seq []]
    (let [way-id (first ways)
          ways (rest ways)]
      (if way-id
        (if-let [way (get way-map way-id)]
          (let [first-node (first (:nodes way))
                last-node (last (:nodes way))]
            (cond
              (nil? end-set)
              (recur
               (conj #{first-node} last-node)
               ways
               (conj connected-way-seq way-id))

              (contains? end-set first-node)
              (recur
               #{last-node}
               ways
               (conj connected-way-seq way-id))

              (contains? end-set last-node)
              (recur
               #{first-node}
               ways
               (conj connected-way-seq way-id))

              :else
              (do
                (println
                 "\tunknown state"
                 (map #(str "n" %) end-set)
                 (str "n" first-node)
                 (str "n" last-node))
                [connected-way-seq false])))
          (do
            (println "\tway lookup failed:" way-id)
            [connected-way-seq false]))
        [connected-way-seq true]))))

(defn resolve-way-geometry-in-memory-go
  "First creates index from nodes on node-in. Afterwards performs node lookup
  of ways on way-in. Adds :coords to ways containing longitude latitude pairs.
  Writes ways to way-out"
  [context node-in way-in way-out]
  (async/go
    (context/set-state context "init")
    (loop [node-map {}
           node (async/<! node-in)]
      (context/set-state context "create-lookup")
      (if node
        (do
          (context/increment-counter context "node-in")
          (recur
           (assoc node-map (:id node) (select-keys node [:longitude :latitude]))
           (async/<! node-in)))
        (loop [way (async/<! way-in)]
          (if way
            (do
              (context/set-state context "resolve-way")
              (context/increment-counter context "way-in")
              (let [coords (map
                            (fn [id]
                              (if-let [node (get node-map id)]
                                node
                                (do
                                  ;; should not happen
                                  (context/increment-counter context "invalid-id")
                                  {:longitude nil :latitude nil})))
                            (:nodes way))]
                (async/>! way-out (assoc way :coords coords))
                (recur (async/<! way-in))))
            (do
              (async/close! way-out)
              (context/set-state context "completion"))))))))

(defn wikipedia-url [wikipedia-tag]
  (let [[lang article] (clojure.string/split wikipedia-tag #":")]
    (str "https://" lang ".wikipedia.org/wiki/"
         (clojure.string/replace article #" " "_"))))

#_(wikipedia-url "it:Palmanova") ;; "https://it.wikipedia.org/wiki/Palmanova"


(defn wikidata-url [wikidata-tag]
  (str "https://www.wikidata.org/wiki/" wikidata-tag))

#_(wikidata-url "Q53302") ;; "https://www.wikidata.org/wiki/Q53302"
