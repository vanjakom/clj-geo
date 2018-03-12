(ns clj-geo.dataset.brompton.dealers
  (:require
    [clj-common.path :as path]
    [clj-common.localfs :as fs]
    [clj-common.json :as json]
    [clj-common.2d :as draw]
    [clj-scraper.scrapers.com.brompton :as scraper]
    [clj-geo.visualization.raster :as raster]
    [clj-geo.visualization.background :as background]

    [clj-geo.env :as env]))

; to be used only once to generate dataset

(defn generate []
  (let [path (path/child
               env/*dataset-path*
               "brompton.com"
               "dataset")]
    (with-open [output-stream (fs/output-stream path)]
      (doseq [dealer (scraper/obtain-dealers)]
        (json/write-to-line-stream dealer output-stream)))))

; to be used when playing with dataset

(defn create-dealers []
  (let [path (path/child
               env/*dataset-path*
               "brompton.com"
               "dataset")]
    (with-open [input-stream (fs/input-stream path)]
      (doall (json/read-lines-keyworded input-stream)))))

(defn create-brompton-locations []
  (filter
    #(and (some? (:name %)) (some? (:longitude %)) (some? (:latitude %)))
    (create-dealers)))

(defn render-brompton-dealers-world-coverage []
  (let [location-convert (partial raster/location->point 18000 9000)
        image-context (background/create-black-region-countries-background {})
        locations (create-brompton-locations)]
    (raster/draw-locations
      {
        :point-size 10
        :image-context image-context}
      locations)
    (with-open [output-stream (fs/output-stream ["tmp" "brompton.png"])]
      (draw/write-png-to-stream image-context output-stream))))



(defn render-brompton-dealers-europe-coverage []
  (let [location-convert (partial raster/location->point 18000 9000)
        image-context (background/create-black-region-countries-background {})
        locations (create-brompton-locations)]
    (raster/draw-locations
      {
        :point-size 10
        :image-context image-context}
      locations)
    (let [europe-image-context (raster/extract-image-context
                                 {
                                   :min-longitude -13
                                   :min-latitude 32
                                   :max-longitude 37
                                   :max-latitude 72}
                                 image-context)]
      (with-open [output-stream (fs/output-stream ["tmp" "brompton.png"])]
        (draw/write-png-to-stream europe-image-context output-stream)))))







; (count (clj-geo.dataset.brompton.dealers/create-dealers))
; 1260
; (frequencies (map :type (clj-geo.dataset.brompton.dealers/create-dealers)))
; {"unknown" 1152, "premier-store" 97, "brompton-junction" 11}


; (count (create-brompton-locations))
; 1167




(comment
  (generate)

  (with-open [input-stream (fs/input-stream (path/child
                                              env/*dataset-path*
                                              "brompton.com"
                                              "dataset"))]
    (doall (json/read-lines-keyworded input-stream))))
