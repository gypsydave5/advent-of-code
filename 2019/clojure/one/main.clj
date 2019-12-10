(defn fuel-count [n]
  (int (- (Math/floor (/ n 3)) 2)))

(apply +
       (map (comp fuel-count clojure.edn/read-string)
            (clojure.string/split-lines (slurp "input.txt"))))
;; => 3262358
;; part two

(defn total-fuel-count [n]
  (loop [current n total 0]
    (let [next (fuel-count current)
          result (+ next total)]
      (if (< next 1)
        total
        (recur next result)))))

(total-fuel-count 10)
;; => 1

(apply +
       (map (comp total-fuel-count clojure.edn/read-string)
            (clojure.string/split-lines (slurp "input.txt"))))
;; => 4890696









