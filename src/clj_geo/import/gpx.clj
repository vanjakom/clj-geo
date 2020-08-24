(ns clj-geo.import.gpx
  (:require
   [clojure.xml :as xml]
   [clj-common.as :as as]
   [clj-common.localfs :as fs]
   [clj-common.path :as path]
   [clj-common.view :as view]))

(let [formatter (new java.text.SimpleDateFormat "yyyy-MM-dd'T'HH:mm:ssX")]
  (.setTimeZone formatter (java.util.TimeZone/getTimeZone "UTC"))
  (defn timestamp->utc-date-time [timestamp]
    (.format formatter (* timestamp 1000)))
  (defn utc-date-time->timestamp [date]
    (.getTime (.parse formatter date))))

#_(defn read [path]
  (xml/parse (fs/input-stream path)))

#_(defn read-stream [input-stream]
  (xml/parse input-stream))

(defn read-track-gpx
  "Intented for general reading of gpx tracks written by Garmin devices.
  Returns following structure
  {
     :wpt-seq [{:lognitude :double :latitude :double :name :string}]
     :track-seq [[{:longitude :double :latitude :double :timestamp :seconds}]]}
  Note: document irregularities"
  [input-stream]
  (let [data (xml/parse input-stream)
        wpt (map
             (fn [data]
               (let [longitude (get-in data [:attrs :lon])
                     latitude (get-in data [:attrs :lat])
                     metadata (view/seq->map :tag (:content data))]
                 {
                  :longitude (as/as-double longitude)
                  :latitude (as/as-double latitude)
                  :name (get-in metadata [:name :content 0])}))
             (filter
              #(= (:tag %) :wpt)
              (:content data)))
        trk (map
             (fn [data]
               (map
                (fn [data]
                  (let [longitude (get-in data [:attrs :lon])
                        latitude (get-in data [:attrs :lat])
                        metadata (view/seq->map :tag (:content data))]
                    {
                     :longitude (as/as-double longitude)
                     :latitude (as/as-double latitude)
                     :timestamp (when-let [date (get-in metadata [:time :content 0])]
                                  (utc-date-time->timestamp date))}))
                (:content data)))
             (filter
              #(= (:tag %) :trkseg)
              (mapcat
               :content
               (filter
                 #(= (:tag %) :trk)
                 (:content data)))))]
    {
     :wpt-seq wpt
     :track-seq trk}))

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
       :attrs {
               "version" "1.1"
               "creator" "trek-mate"
               "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance"
               "xmlns" "http://www.topografix.com/GPX/1/1"
               "xsi:schemaLocation" "http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd"}
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
                        :attrs {:lon (str (:longitude location))
                                :lat (str (:latitude location))}
                        :content (filter
                                  some?
                                  [ ;; todo format timestamp
                                   (when-let [timestamp (:timestamp location)]
                                     {:tag :time :content [(timestamp->utc-date-time timestamp)]})
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
