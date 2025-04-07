(ns clj-geo.dataset.geonames.countries
  (:require
    [clj-common.path :as path]
    [clj-common.io :as io]
    [clj-common.logging :as logging]
    [clj-common.localfs :as fs]
    [clj-geo.env :as env]
    [clj-geo.dataset.geonames.common :as common]))

; ISO	ISO3	ISO-Numeric	fips	Country	Capital	Area(in sq km)	Population	Continent	tld	CurrencyCode	CurrencyName	Phone	Postal Code Format	Postal Code
(def countries-header
  [
    [(common/name->keyword "iso") identity]
    [(common/name->keyword "iso3") identity]
    [(common/name->keyword "iso-numeric") common/optional-long]
    [(common/name->keyword "fips") identity]
    [(common/name->keyword "country") identity]
    [(common/name->keyword "capital") identity]
    [(common/name->keyword "area-sq-km") common/optional-double]
    [(common/name->keyword "population") common/optional-long]
    [(common/name->keyword "continent") identity]
    [(common/name->keyword "tld") identity]
    [(common/name->keyword "currency-code") identity]
    [(common/name->keyword "currency-name") identity]
    [(common/name->keyword "phone") identity]
    [(common/name->keyword "postal-code-format") identity]
    [(common/name->keyword "postal-code-regex") identity]
    [(common/name->keyword "languages") identity]
    [(common/name->keyword "geonameid") common/optional-long]
    [(common/name->keyword "neighbours") identity]
    [(common/name->keyword "equivalent-fips-code") identity]])

(def continent-lookup
  {
    "SA" "South America"
    "EU" "Europe"
    "AN" "Antartica"
    "OC" "Oceania"
    "AF" "Africa"
    "AS" "Asia"
    "NA" "North America"})

(defn create-countries []
  (with-open [countries-stream (fs/input-stream
                                 (path/child
                                   env/*dataset-path*
                                   "geonames.org"
                                   "GazetteerData"
                                   "countryInfo.txt"))]
    (let [lines (line-seq (io/input-stream->buffered-reader countries-stream))]
      (doall
        (map
          #(assoc % :continent-name (continent-lookup (:continent %)))
          (map
            (partial
              common/tsv-line->typed-map
              countries-header)
            (filter
              #(not (.startsWith % "#"))
              lines)))))))




(comment
  (run! logging/report (take 5 (create-countries)))

  (run! logging/report (into #{} (map :continent (create-countries)))))
