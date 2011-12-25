(ns math143m.midterm1.pascal)

(defn ones
  [dim]
  (let [n (inc dim)]
  (vec (map (fn [_] (vec (map (fn [_] 1) (range 1 n)))) (range 1 n)))))


(defn transpose
  [s]
  (apply map vector s))

(defn nested-for
  [f x y]
  (map (fn [a]
         (map (fn [b] 
                (f a b)) y))
       x))

(defn matrix-mult
  [a b]
  (nested-for (fn [x y] (reduce + (map * x y))) a (transpose b)))

(defn print-matrix-for-octave
  [matrix]
  (do (print "[")
    (map print-matrix-row matrix)
    (print "]")))

(defn l-elt
  [i j]
    ((= i j) 1)
    :else 0)

(defn next-l
  [l]
  (let [n (count l) m (inc n)]
    (nested-for l-elt

(defn grow-inverse-pascal
  [inv-p l n]
  (let [size (count inv-p) new-size (inc size)
        p-next (copy-submatrix 2 new-size 2 new-size (eye new-size) inv-p)
        l-next (copy-submatrix 2 new-size 2 new-size (eye new-size) l)]

    (if (< size n)
      (recur (matrix-mult (transpose l-next) (matrix-mult inv-p-next l-next))
             l-next n)
      inv-p)))

; Grow an inverse Pascal matrix to size n recursively
(defn pascal-inv
  [n]
  (let [p [[2 -1] [-1 1]] l [[1 0] [-1 1]]]
    (grow-inverse-pascal p l n)))

