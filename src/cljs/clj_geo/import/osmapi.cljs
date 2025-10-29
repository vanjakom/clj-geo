(ns clj-geo.import.osmapi)

;; subset of functions available in java version, try to be consistent
;; to enable drop in replacement

(def ^:dynamic *server* "https://api.openstreetmap.org")

;; 20250720 tried with defining dataset functon ( node-full ... ) and use
;; fetch in Java Script but JSContext ( JS env in trek-mate ios is not
;; supporting it ) went with  doing request in swift and passing data to
;; CLJS to do reasoning, maybe return to this when trek-mate-web is
;; implemented

#_(defn node-full [id]
    (.then
     (js/fetch (str *server* "/api/0.6/node/" id) ".json")
     ;; todo something is failing
     (fn [resp] (.json resp))))
