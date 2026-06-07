(ns clj-geo.dot.store.humandot
  (:require
   [clj-common.as :as as]
   [clj-common.io :as io]
   [clj-common.localfs :as fs]
   [clj-common.path :as path]))

;; use /Users/vanja/dataset-git/dots/template.dot
;; to create new human-dots

;; format definition
;; file should start with [humandot]
;; after first line additional customization could be set in "[statement]" form
;; where content of statement is arbitrary
;; 
;; comments are starting with ;
;; 
;; location should be longitude, latitude pair
;; if location should be extracted use @ at start
;; 
;; tags and notes should be in lines after location indented with space(s) or tab
;; tags should start with #
;; reference to osm as osm website link ( use https when possible )
;; https://www.openstreetmap.org/relation/11097832
;; --- line in tags should be used to separate extracted from added tags
;; === line in tags should be used to separate public tags from private one
;; public vs private is more meant for visiblity ( what to be visible on public
;; and what on private map )
;; 
;; location extraction ( useful for references originally coming from osm ) should
;; happen only once ( replacing @ with longitude, latitude pair ) using either
;; specific extractor or generic one which will use location from first line
;; that can be extracted
;; 
;; it's nice practice to add comment about tags used
;; tags used
;;    #tag1 - generic tag


;; todo support "@" for location without coordinates, to be extracted

(defn create-dot [longitude latitude tags]
  {
   :longitude longitude
   :latitude latitude
   ;; vector of tags in order given in file
   :tags tags})

(defn read [is]
  ;; go over lines, skip comments and empty lines
  ;; extract default-tags and other directives
  ;; extract locations
  ;; at the end apply directives ( default tags ... )
  (let [[locations location default-tags]
        (reduce
         (fn [[locations location default-tags] line]
           #_(println "[" (count locations) "]" location)
           (cond
             ;; comment
             (.startsWith line ";")
             [locations location default-tags]

             ;; [humandot]
             (= line "[humandot]")
             [locations location default-tags]

             ;; [statement]
             (and (.startsWith line "[") (.endsWith line "]"))
             (let [statement (.substring line 1 (dec (.length line)))]
               (if (.startsWith statement "tag:")
                 (let [tag (.substring statement 4)]
                   [locations location (conj default-tags tag)])
                 [locations location default-tags]))
             
             ;; empty line
             (and (clojure.string/blank? line) (some? location))
             [(conj locations location) nil default-tags]
             (and (clojure.string/blank? line) (nil? location))
             [locations nil default-tags]

             ;; tag
             (or (.startsWith line " ") (.startsWith line "\t"))
             (let [tag (.trim line)]
               [
                locations
                (update-in location [:tags] #(conj % tag))
                default-tags])

             ;; 20260304 initially plan was to have location placeholder which
             ;; would be dynamically populated but it's abandoned
             ;; start new location
             ;;(= line "@")
             ;;[locations {} tags]

             ;; not tag, then if has , it's location
             (.contains line ",")
             (let [fields (.split line ",")
                   longitude (as/as-double (get fields 0))
                   latitude (as/as-double (get fields 1))]
               [locations
                (create-dot longitude latitude [])
                default-tags])

             ;; skip
             :else
             [locations location default-tags]))
         ;; 20260304 fixing bug with tags being set, must be vector
         [[] nil []]
         (io/input-stream->line-seq is))]
    (let [final-locations (if (some? location)
                            (conj locations location)
                            locations)]
      ;; add default tags to the end
      (map
       (fn [location]
        (update-in location [:tags] #(concat % default-tags)))
       final-locations))))

(defn print [is]
  (let [locations (read is)]
    (doseq [location locations]
      (println (:longitude location) "," (:latitude location))
      (doseq [tag (:tags location)]
        (println "\t" tag)))))

(defn write-to-string [dot]
  (str
   (:longitude dot) ", " (:latitude dot) "\n"
   (reduce
    (fn [buffer tag]
      (str buffer "   " tag "\n"))
    ""
    (:tags dot))))

(defn write [os header-line-seq dot-seq]
  (io/write-line os "[humandot]")
  (io/write-new-line os)
  (doseq [header-line header-line-seq]
    (io/write-line os (str "; " header-line)))
  (io/write-new-line os)
  (doseq [dot dot-seq]
    (io/write-line os (write-to-string dot))
    (io/write-new-line os)))


(defn tags-as-set [location]
  (assoc
   location
   :tags
   (into #{} (:tags location))))

;; todo
;; support for processing and extractors



#_(println (write-to-string
            {:longitude 20 :latitude 44 :tags ["Location 1" "#test"]}))

#_(with-open [is (fs/input-stream
                ["Users" "vanja" "dataset-git" "dots" "template.dot"])]
  (print is))

#_(with-open [is (fs/input-stream
                ["Users" "vanja" "dataset-git" "dots" "camps.dot"])]
  (print is))
