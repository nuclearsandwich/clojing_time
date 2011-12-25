(def world
  [[   1   1   1   1   1 ]
   [ 999 999 999 999   1 ]
   [   1   1   1   1   1 ]
   [   1 999 999 999 999 ]
   [   1   1   1   1   1 ]])

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size xy]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx))
           (map #(map + yx %) deltas))))

(defn estimate-cost [step-cost-est size y x]
  (* step-cost-est
     (- (+ size size) y x 2)))
