(ns clj-geo.dataset.opencacheuk)

(require '[clj-common.path :as path])
(require '[clj-geo.import.okapi :as okapi])
(require '[clj-geo.env :as env])
(require '[clj-common.logging :as logging])

(require '[clj-common.reducers :as r])

(def ^:const full-dump-path ["opencache.uk"])

(defn full-dump-metadata []
  (okapi/full-dump-metadata
    (apply
      path/child
      env/*dataset-path*
      full-dump-path)))

(defn full-dump-seq []
  (okapi/full-dump-seq
    (full-dump-metadata)))

(comment
  (def metadata (full-dump-metadata))

  metadata

  (first (into [] (r/take 1 (okapi/stream-full-dump metadata))))

  (r/first (okapi/stream-full-dump metadata))

  (r/count (okapi/stream-full-dump metadata))

  (logging/report (r/first (stream-full-dump)))

  (logging/report (last (take 10000 (stream-full-dump))))

  (logging/report (first (full-dump-seq)))

  (logging/report (first (filter #(not (= (:object_type %1) "geocache")) (full-dump-seq))))

  (def cache (first (full-dump-seq)))

  (keys (:data cache))


  (:gc_code (:data cache))

  (:code (:data cache))

  (:location (:data cache))

  (assoc cache :data nil)

  (logging/report (update-in cache [:data :descriptions] (fn [_] nil)))


  (count-by {} {:object_type "test"})

  (logging/report
    (transduce
      okapi/geocache-dump-transform
      (completing
        (fn [state object]
          (let [key (okapi/type object)]
            (assoc state key (inc (get state key 0))))))
      {}
      (okapi/full-dump-parts metadata)))


  (logging/report
    (first
      (transduce
        (comp
          okapi/geocache-dump-transform
          (take 1))
        conj
        (okapi/full-dump-parts metadata))))


  (logging/report
    (transduce
      okapi/full-dump-transform
      (completing
        (fn [state object]
          (let [key (:change_type object)]
            (assoc state key (inc (get state key 0))))))
      {}
      (okapi/full-dump-parts metadata)))

  ; something is wrong with this transducer, I should use xf to delegate calls
  (defn count-by [xf]
    (fn
      ([] {})
      ([state] state)
      ([state object]
       (println "calling reduce")
       (if-let [key (:status object)]
         (assoc state key (inc (get state key 0)))
         state))))


  (logging/report
    (transduce
      okapi/geocache-dump-transform
      (completing (fn [state object]
                    (if-let [key (:status object)]
                      (assoc state key (inc (get state key 0)))
                      state)))
      {}
      (okapi/full-dump-parts metadata)))

  ; not working
  (logging/report
    (transduce
      (comp
        okapi/geocache-dump-transform
        count-by)
      conj
      []
      (okapi/full-dump-parts metadata)))



  (count
    (transduce
      okapi/full-dump-transform
      conj
      (okapi/full-dump-parts metadata)))




  (transduce
    (comp
      (comp
        (filter even?)
        (map inc))
      (map (partial * 2)))
    conj
    (range 10))

  (transduce
    (comp
      (map #(into {} (map (fn [[key value]] [(keyword key) value]) %1))))
    conj
    '({"type" "a"} {"type" "b"} {"type" "a"}))




  (logging/report
    (r/count-by
      :change_type
      (stream-full-dump)))


)

