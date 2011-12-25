(defn fibs [n]
  (cond
    (= 1 n) [1]
    (= 2 n) [1 1]
    :else (let [old-fibs (fibs (dec n))]
            (conj old-fibs (+ (peek old-fibs) (peek (pop old-fibs)))))))

(defn tail-fibs [n]
  ())

(println (fibs 1))
(println (fibs 2))
(println (fibs 8))
; (1 1 2 3 5 8 13 21)
