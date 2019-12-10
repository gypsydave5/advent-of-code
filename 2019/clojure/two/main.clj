(def input
  (into-array
   (map clojure.edn/read-string
        (clojure.string/split "1,9,10,3,2,3,11,0,99,30,40,50" #","))))

(defn fresh-input []
  (into-array
   (map clojure.edn/read-string
        (clojure.string/split (slurp "input.txt") #","))))


(aset input 1 12)
(aset input 2 2)

(defn doit [noun verb]
  (let [input (fresh-input)]
    (aset input 1 noun)
    (aset input 2 verb)
    (loop [p 0]
      (let [command (get input p)]
        (cond
          (= command 1) (do
                          (aset input (get input (+ p 3)) (+ (get input (get input (+ p 1))) (get input (get input (+ p 2))))))
          (= command 2) (do
                          (aset input (get input (+ p 3)) (* (get input (get input (+ p 1))) (get input (get input (+ p 2)))))))
        (if (= command 99)
          (get input 0)
          (recur (+ p 4)))))))

;; part two
(defn cart [colls]
  (if (empty? colls)
    '(())
    (for [more (cart (rest colls))
          x (first colls)]
      (cons x more))))

(filter (fn [[r x]] (= r 19690720))
        (pmap (fn [[n v]] [(doit n v) (+ (* 100 n) v)])
              (cart [(range 0 100) (range 0 100)])))
;; => ([19690720 9820])
