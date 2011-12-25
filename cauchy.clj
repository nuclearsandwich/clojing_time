(ns math143m.midterm1.cauchy)

; The ideal solution for this Cauchy matrix is to work with a language or
; environment with first class support for rationals. Then we need only be
; concerned with the relative size of the denominators, which for a 100 by 100
; Cauchy matrix C_ij = (1 / (i + j)) is only 100 and likely wouldn't grow beyond
; a distance of 10e±16 from 1.
(defn cauchy
  "Generates an n x n Cauchy matrix (C = 1/(i + j))"
  [n]
  (let [cauchy-elt (fn [i j] (/ 1 (+ i j)))
          cauchy-row (fn [i max-j]
                       (vec (map #(cauchy-elt i %) (range 1 (inc max-j)))))]
    (vec (map #(cauchy-row % n) (range 1 (inc n))))))

(def cauchy-100 (cauchy 100))

(defn print-matrix [matrix]
  (map println matrix))

(defn cauchy-inv-elt [i j n]
  (let [x (into (vec (range 1 i)) (vec (range (inc i) (inc n))))
        y (into (vec (range 1 j)) (vec (range (inc j) (inc n))))
        total (vec (range 1 (inc n)))]
    (/ (* (int (Math/pow -1 (+ i j))) (cauchy-det x y)
          (cauchy-det total total)))))

(defn cauchy-inv-row [i n]
  (vec (map (fn [j] (cauchy-inv-elt i j n)) (range 1 (inc n)))))

(defn cauchy-inv [n])

; The determinant of a Cauchy matrix C(i,j)=1/(x_i+y_j) is
; det C = [\prod i< j(x_j-x_i)(y_j-y_i)] / [\prod i,j (x_i+y_j)].
(defn cauchy-det
  [x y]
  (reduce (fn [d i] (reduce (fn
                              [d j]
                              (/ (if (< i j)
                                   (* d (- i j) (- i j)) d) (+ i j)))
                            d y))
          1 x))

