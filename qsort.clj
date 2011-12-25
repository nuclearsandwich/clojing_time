(ns nuclearsandwich.quicksort)

(defn nom [n] (take n (repeatedly #(rand-int n))))

(defn sort-parts 
  "Lazy recursive incremental quicksort. Works against partitions created based
  on pivot"
  [work]
  (lazy-seq
    (loop [[part & parts] work]
      (println "part:" part "parts:" parts)
      (if-let [[pivot & xs] (seq part)]
        (let [smaller? #(< % pivot)]
          (println "pivot:" pivot "xs:" xs)
          (recur (list*
                   (filter smaller? xs)
                   pivot
                   (remove smaller? xs)
                   parts)))
        (when-let [[x & parts] parts]
          (println "x:" x "parts:" parts)
          (println)
          (println)
          (cons x (sort-parts parts)))))))

(defn qsort [xs] (sort-parts (list xs)))

(println (first (qsort (nom 10))))

