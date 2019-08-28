(ns clj-geo.import.location
  (:require
   [clj-common.as :as as]
   [clj-common.test :as test]))

(defn string->location
  [location-string]
  (first
   (filter
    some?
    (map
     #(%)
     [
      #(if-let [matches
                (re-matches
                 ;; N 35° 53.475, E 14° 30.426
                 #"^(N|S).*?([0-9]+).*?([0-9]+)\.([0-9]+).*?(E|W).*?([0-9]+).*?([0-9]+).([0-9]+)$"
                location-string)]
         (let [latitude (+
                          (as/as-double (nth matches 2))
                          (/
                           (as/as-double (str (nth matches 3) "." (nth matches 4)))
                           60))
               longitude (+
                         (as/as-double (nth matches 6))
                         (/
                          (as/as-double (str (nth matches 7) "." (nth matches 8)))
                          60))]
           {
            :longitude (if (= "E" (nth matches 5)) longitude (- longitude))
            :latitude (if (= "N" (nth matches 1)) latitude (- latitude))}))]))))

(test/test
 "location convert test 1"
 (=
  (string->location "N 35° 53.475, E 14° 30.426")
  {:longitude 14.5071, :latitude 35.89125}))

