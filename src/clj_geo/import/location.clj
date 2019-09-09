(ns clj-geo.import.location
  (:require
   [clj-common.as :as as]
   [clj-common.test :as test]))

(def minute-formatter-fn
  (let [formatter (new java.text.DecimalFormat "0.000")]
    (fn [degrees]
      (.format formatter degrees))))

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

(defn location->string
  [{longitude :longitude latitude :latitude}]
  (let [longitude-abs (Math/abs longitude)
        longitude-sign (if (> longitude 0) "E" "W")
        longitude-degree (as/as-long longitude)
        longitude-minute (* (- longitude longitude-degree) 60)
        latitude-abs (Math/abs latitude)
        latitude-sign (if (> latitude 0) "N" "S")
        latitude-degree (as/as-long latitude)
        latitude-minute (* (- latitude latitude-degree) 60)]
    (str
     latitude-sign " " latitude-degree "° " (minute-formatter-fn latitude-minute) " "
     longitude-sign " " longitude-degree "° " (minute-formatter-fn longitude-minute))))

(test/test
 "location convert test 1"
 (=
  (string->location "N 35° 53.475, E 14° 30.426")
  {:longitude 14.5071, :latitude 35.89125}))

(test/test
 "serialize / deserialize"
 (=
  "N 35° 53.475 E 14° 30.426"
  (location->string
   (string->location "N 35° 53.475, E 14° 30.426"))))
