(defn fac
  "returns the factorial of n"
  [n]
  (if (<= n 1)
    1
    (* n (fac (- n 1)))))

(defn tail-fac
  "Tail recursive function to return the factorial of n"
  [n]
  ((fn [n acc]
    (if (<= n 1)
      (* acc 1)
      (recur (dec n) (* acc n)))) n 1))
