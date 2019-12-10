(defn parse-direction [s]
  (let [direction (symbol (str (first s)))
        distance (clojure.edn/read-string (clojure.string/join (rest s)))]
    (cond (= direction 'R) [0 distance]
          (= direction 'L) [0 (- distance)]
          (= direction 'U) [distance 0]
          (= direction 'D) [(- distance) 0])))

(def input (map #(map parse-direction %)
                (map #(clojure.string/split % #",")
                     (clojure.string/split-lines (slurp "input.txt")))))

(defn relative-range [start diff]
  (if (neg? diff)
    (range (dec start) (+ start diff -1) -1)
    (range (inc start) (+ start diff 1) 1)))

(defn move [origin offset]
  (concat (map (fn [m] [ m (second origin)])
               (relative-range (first origin) (first offset)))
          (map (fn [m] [(first origin) m])
               (relative-range (second origin) (second offset)))))

(defn go [points directions]
  (if (empty? directions) points
      (recur (concat points
                     (move (last points) (first directions)))
             (rest directions))))

(defn manhattan [[ax ay] [bx by]]
  (+ (Math/abs (- ax bx))
     (Math/abs (- ay by))))

(defn intersections [paths]
  (apply clojure.set/intersection (map #(set (rest %)) paths)))

(defn follow-directions [directions]
  (go [[0 0]] directions))

(def distance-from-origin
  (partial manhattan [0 0]))

(defn closest-intersection [directions]
  (apply min
         (map distance-from-origin
              (intersections (map follow-directions directions)))))

(time (closest-intersection input))
;; => "Elapsed time: 2079.405838 msecs"
;;
;;    209

;; part two

(defn distance-to-point [point path]
  (loop [i 0 path path]
    (if (= point (first path))
      i
      (recur (inc i) (rest path)))))

(defn shortest-intersection [input]
  (let [paths (pmap follow-directions input)
        intersections (intersections paths)]
    (apply min (pmap
                #(apply + (map (partial distance-to-point %) paths))
                intersections))))

(time (shortest-intersection input))
;; => "Elapsed time: 2170.61238 msecs"
;;
;;    43258













