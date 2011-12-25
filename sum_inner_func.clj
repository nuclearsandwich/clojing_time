(defn sum-down-from [x]
  (let [current-sum 0]
    ((fn sum-helper [sum x]
      (if (pos? x)
        (recur (+ sum x) (dec x))
        sum)) current-sum x)))
(println (sum-down-from 6))
