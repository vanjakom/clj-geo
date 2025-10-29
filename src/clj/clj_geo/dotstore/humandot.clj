(ns clj-geo.dotstore.humandot
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

(defn location [longitude latitude tags]
  {
   :longitude longitude
   :latitude latitude
   :tags tags})

(defn read [is]
  (let [[locations location tags]
        (reduce
         (fn [[locations location tags] line]
           #_(println "[" (count locations) "]" location)
           (cond
             ;; comment
             (.startsWith line ";")
             [locations location tags]

             ;; [humandot]
             (= line "[humandot]")
             [locations location tags]

             ;; [statement]
             (and (.startsWith line "[") (.endsWith line "]"))
             (let [statement (.substring line 1 (dec (.length line)))]
               (if (.startsWith statement "tag:")
                 (let [tag (.substring statement 4)]
                   [locations location (conj tags tag)])
                 [locations location tags]))
             
             ;; empty line
             (and (clojure.string/blank? line) (some? location))
             [(conj locations location) nil tags]
             (and (clojure.string/blank? line) (nil? location))
             [locations nil tags]

             ;; tag
             (or (.startsWith line " ") (.startsWith line "\t"))
             (let [tag (.trim line)]
               (if (and
                    (not (contains? tags tag))
                    ;; special tag to divide extracted tags from added ones
                    ;; 20250226 leaving it in tags to support parsing of it
                    ;; and private tags section
                    ;; added public tags section start
                    #_(not (= tag "---"))
                    ;; added private tags section start
                    #_(not (= tag "===")))
                 [
                  locations
                  (update-in location [:tags] #(conj (or % []) tag))
                  tags]
                 ;; do not add tag if already on global
                 [
                  locations
                  location
                  tags]))

             (= line "@")
             ;; start new location
             [locations {} tags]

             ;; not tag, then if has , it's location
             (.contains line ",")
             (let [fields (.split line ",")
                   longitude (as/as-double (get fields 0))
                   latitude (as/as-double (get fields 1))]
               [locations
                {
                 :longitude longitude
                 :latitude latitude
                 :tags (into [] tags)}
                tags])

             :else
             ;; skip
             [locations location tags]))
         [[] nil #{}]
         (io/input-stream->line-seq is))]
    (if (some? location)
      (conj locations location)
      locations)))

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




