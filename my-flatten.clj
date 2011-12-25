(defn my-flatten [s]
  (let [flatten-one-layer
        (fn [flat unk]
          (cond
            (empty? unk) (seq flat)
            (seq? (first unk)) (recur (into flat (first unk)) (rest unk))
            :else (recur (conj flat (first unk)) (rest unk))))
        sq (map #(if (vector? %) (seq %) %) s)]
        (if (not (apply = (conj (map seq? sq) false)))
          (recur (flatten-one-layer [] sq))
          sq)))

(println (my-flatten '(1 2 3 4 (\a \b \c (\x \y \z)))))

(println (my-flatten '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
