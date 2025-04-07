(ns clj-geo.import.gpx
  (:require
   [clojure.xml :as xml]
   [clj-common.as :as as]
   [clj-common.localfs :as fs]
   [clj-common.path :as path]
   [clj-common.view :as view]))

(let [formatter-1 (new java.text.SimpleDateFormat "yyyy-MM-dd'T'HH:mm:ssX")
      formatter-2 (new java.text.SimpleDateFormat "yyyy-MM-dd'T'HH:mm:ss.SSSX")]
  (.setTimeZone formatter-1 (java.util.TimeZone/getTimeZone "UTC"))
  (.setTimeZone formatter-2 (java.util.TimeZone/getTimeZone "UTC"))
  (defn timestamp->utc-date-time [timestamp]
    (.format formatter-1 (* timestamp 1000)))
  (defn utc-date-time->timestamp [date]
    (try
      (.getTime (.parse formatter-1 date))
      (catch Exception e
        (.getTime (.parse formatter-2 date))))))

#_(defn read [path]
  (xml/parse (fs/input-stream path)))

#_(defn read-stream [input-stream]
  (xml/parse input-stream))

(defn read-gpx
  "Intented for general reading of gpx tracks written by Garmin devices.
  Returns following structure
  {
     :wpt-seq [{:lognitude :double :latitude :double :name :string}]
     :track-seq [[{:longitude :double :latitude :double :timestamp :seconds}]]
     :route-seq [[{:longitude :double :latitude :double :timestamp :seconds}]]}

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
                  :name (get-in metadata [:name :content 0])
                  ;; garmin support
                  :symbol (get-in metadata [:sym :content 0])}))
             (filter
              #(= (:tag %) :wpt)
              (:content data)))
        ;; todo check concatenation
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
                (:content data)))))
        rte (map
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
                (filter
                 #(= (:tag %) :rtept)
                 (:content data))
                ))
             (filter
              #(= (:tag %) :rte)
              (:content data)))]
    {
     :wpt-seq wpt
     :track-seq trk
     :route-seq rte}))
(def read-track-gpx read-gpx)

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

(defn write-gpx
  "Used for writing of complex GPX files, containing waypoints, routes, tracks"
  [output-stream feature-seq]
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
       feature-seq}))
    (.flush writer)))

(defn waypoint
  [longitude latitude symbol name description]
  {
   :tag :wpt
   :attrs {"lat" latitude "lon" longitude}
   :content
   (filter
    some?
    [
     (when symbol
       {:tag :sym :content [symbol]})
     (when name
       {:tag :name :content [name]})
     (when description
       {:tag :desc :content [description]})
     ])})

(defn route
  [name description route-point-seq]
  {
   :tag :rte
   :content
   (concat
    (filter
     some?
     [
      (when name
        {:tag :name :content [name]})
      (when description
        {:tag :desc :content [description]})])
    route-point-seq)})


(defn route-point
  [longitude latitude symbol name description]
  {
   :tag :rtept
   :attrs {"lat" latitude "lon" longitude}
   :content
   (filter
    some?
    [
     (when symbol
       {:tag :sym :content [symbol]})
     (when name
       {:tag :name :content [name]})
     (when description
       {:tag :desc :content [description]})
     ])})

(defn track
  [track-seq]
  {
   :tag :trk
   :content track-seq})

(defn track-segment
  [location-seq]
  {
   :tag :trkseg
   :content
   (map
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
    location-seq)})

#_(with-open [os (fs/output-stream ["tmp" "test.gpx"])]
  (write-gpx
   os
   [
    (waypoint 20 44 nil "Beograd" nil)]))

#_(with-open [output-stream (clj-common.localfs/output-stream ["tmp" "text.gpx"])]
  (write-track-gpx
  output-stream
  #{"test" "ride"}
  [{:longitude 10 :latitude 15 :timestamp 1} {:longitude 20 :latitude 30 :timestamp 2}]))
