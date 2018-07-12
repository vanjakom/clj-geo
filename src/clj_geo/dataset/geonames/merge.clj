(ns clj-geo.dataset.geonames.merge
  (:require
    [clj-common.logging :as logging]
    [clj-geo.dataset.geonames.countries :as countries]
    [clj-geo.dataset.geonames.cities :as cities]))


(defn merge-cities-with-country [city-seq]
  (let [countries (reduce
                    (fn [state country]
                      (assoc
                        state
                        (:iso country)
                        country))
                    {}
                    (countries/create-countries))]
    (map
      #(let [country (countries (:country-code %))]
         (assoc
           %
           :country-name (:country country)
           :continent-name (:continent-name country)))
      city-seq)))

(def create-cities-1000-with-country (partial merge-cities-with-country (cities/create-cities-1000)))

(def create-cities-5000-with-country (partial merge-cities-with-country (cities/create-cities-5000)))

(def create-cities-15000-with-country (partial merge-cities-with-country (cities/create-cities-15000)))



(comment
  (count (create-cities-1000-with-country))
  (count (create-cities-5000-with-country))
  (count (create-cities-15000-with-country)))
