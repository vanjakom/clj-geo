(ns clj-geo.dot.core)

;; dot represents location. location is defined with longitude, latitude and
;; tags. tags are vector. could be anyting, multiline text or label. if tag
;; is label it usually starts with # (public tag) or @ (personal tag), it could
;; represent key value pair then it has format |key|value

(defn create
  "longitude, latitude as double. tags vector of strings"
  [longitude latitude tags]
  {
   :longitude longitude
   :latitude latitude
   :tags tags})

(defn append-tag [dot tag]
  (update-in dot [:tags] conj tag))

(defn longitude [dot] (:longitude dot))

(defn latitude [dot] (:latitude dot))

(defn tags [dot] (:tags dot))

(defn tags-as-set [dot] (into #{} (:tags dot)))

#_(append-tag (create 44 20 ["Beograd"]) "glavni grad Srbije")
