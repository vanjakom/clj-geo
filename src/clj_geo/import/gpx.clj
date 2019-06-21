(ns clj-geo.import.gpx
  (:require [clojure.xml :as xml])
  (:require [clj-common.path :as path])
  (:require [clj-common.localfs :as fs])
  (:require [clj-common.view :as view]))

(let [formatter (new java.text.SimpleDateFormat "yyyy-MM-dd'T'HH:mm:ss'Z'")]
  (.setTimeZone formatter (java.util.TimeZone/getTimeZone "UTC"))
  (defn timestamp->utc-date-time [timestamp]
    (.format formatter (* timestamp 1000))))

(defn read [path]
  (xml/parse (fs/input-stream path)))

(defn read-stream [input-stream]
  (xml/parse input-stream))

(defn write-track-gpx
  "Writes GPX to OutputStream. Tags will be written inside metadata tag.
  Each location should have following keys :longitude :latitude :timestamp
  :elevation :location-accuracy :elevation-accuracy. Longitude, latitude and
  timestamp are required. Timestamp should be in seconds.
  Note: StackOverflowException when used System/out as OutputStream"
  [output-stream tag-seq location-seq]
  (let [writer (new java.io.OutputStreamWriter output-stream)]
    (binding [*out* writer]
     (xml/emit
      {
       :tag :gpx
       :content
       [
        {:tag :metadata
         :content (map
                   (fn [tag]
                     {:tag "tag" :content [tag]})
                   tag-seq)}
        {
         :tag :trk
         :content
         [
          {:tag :trkseg
           :content (map
                     (fn [location]
                       {:tag :trkpt
                        :attrs {:longitude (str (:longitude location))
                                :latitude (str (:latitude location))}
                        :content (filter
                                  some?
                                  [ ;; todo format timestamp
                                   {:tag :time :content [(timestamp->utc-date-time
                                                          (:timestamp location))]}
                                   (when-let [elevation (:elevation location)]
                                     {:tag :ele :content [elevation]})
                                   (when-let [accuracy (:location-accuracy location)]
                                     (:tag :location-accuracy :content [(str accuracy)]))
                                   (when-let [accuracy (:elevation-accuracy location)]
                                     (:tag :elevation-accuracy :content [(str accuracy)]))])})
                     location-seq)}]}]}))
    (.flush writer)))

#_(with-open [output-stream (clj-common.localfs/output-stream ["tmp" "text.gpx"])]
  (write-track-gpx
  output-stream
  #{"test" "ride"}
  [{:longitude 10 :latitude 15 :timestamp 1} {:longitude 20 :latitude 30 :timestamp 2}]))





