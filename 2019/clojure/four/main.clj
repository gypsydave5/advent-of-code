(def numbers (range 347312 805915))

(count numbers)
;; => 458603

(take 10 numbers)
;; =>
;; (347312 347313 347314 347315 347316 347317 347318 347319 347320 347321)

(defn increasing-digits? [n]
  (apply <= (map (comp read-string str) (str n))))

(defn doubled-digit? [n]
  (some (partial apply =) (partition 2 1 (str n))))

(count (filter (fn [n] (and (increasing-digits? n) (doubled-digit? n))) numbers))
;; => 594

;; part-two

(defn group-same [seq]
  (loop [s (rest seq) r [[(first seq)]]]
    (cond (empty? s) r
          (= (last (last r)) (first s)) (recur (rest s) (conj (vec (butlast r))
                                                              (vec (conj (last r) (first s)))))
          :else (recur (rest s) (conj r [(first s)])))))

(defn doubled-no-treble? [n]
  (some (fn [n] (= 2 (count n)))
        (group-same (str n))))

(count (filter (fn [n] (and (increasing-digits? n) (doubled-no-treble? n))) numbers))
;; => 364