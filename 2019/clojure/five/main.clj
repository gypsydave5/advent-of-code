(ns main)



(defn parse-instruction [i]
  {:opcode (Integer/parseInt (apply str (take-last 2 (str i))))
   :pmodes (map (comp #(Integer/parseInt %) str) (reverse (drop-last 2 (str i))))})

(defn get-parameter [program i mode]
  (cond (= mode 0) (get program (get program i))
        (= mode 1) (get program i)))

(defn get-parameters [program i pmodes count]
  (let [pmodes (right-pad pmodes count 0)]
    (map (fn [n pmode] (get-parameter program n pmode))
         (range (+ 1 i) (+ 1 i count))
         pmodes)))

(defn right-pad [col len pad]
  (take len (concat col (repeat pad))))

(defn add [program i pmodes inputs]
  (let [[a b] (get-parameters program i pmodes 2)]
    (aset program (get program (+ i 3)) (+ a b)))
  {:output [] :next-i (+ i 4) :inputs inputs})

(defn multiply [program i pmodes inputs]
  (let [[a b] (get-parameters program i pmodes 2)]
    (aset program (get program (+ i 3)) (* a b)))
  {:output [] :next-i (+ i 4) :inputs inputs})

(defn use-input [program i pmodes inputs]
  (aset program (get program (+ i 1)) (first inputs))
  {:output [] :next-i (+ i 2) :inputs (rest inputs)})

(defn write-output [program i pmodes inputs]
  (let [[a] (get-parameters program i pmodes 1)]
    {:output [a]
     :next-i (+ i 2)
     :inputs inputs}))

(defn jump-if-true [program i pmodes inputs]
  (let [[a b] (get-parameters program i pmodes 2)]
    {:output []
     :next-i (if (not= 0 a) b (+ i 3))
     :inputs inputs}))

(defn jump-if-false [program i pmodes inputs]
  (let [[a b] (get-parameters program i pmodes 2)]
    {:output []
     :next-i (if (= 0 a) b (+ i 3))
     :inputs inputs}))

(defn less-than [program i pmodes inputs]
  (let [[a b] (get-parameters program i pmodes 2)]
    (aset program (get program (+ i 3)) (if (< a b) 1 0))
    {:output [] :next-i (+ i 4) :inputs inputs}))

(defn equals [program i pmodes inputs]
  (let [[a b] (get-parameters program i pmodes 2)]
    (aset program (get program (+ i 3)) (if (= a b) 1 0))
    {:output [] :next-i (+ i 4) :inputs inputs}))

(defn execute [program inputs]
  (loop [i 0 inputs inputs outputs []]
    (let [instruction (get program i)
          {:keys [opcode pmodes]} (parse-instruction instruction)
          f (condp = opcode
              1 add
              2 multiply
              3 use-input
              4 write-output
              5 jump-if-true
              6 jump-if-false
              7 less-than
              8 equals
              99 (constantly nil))
          {:keys [output next-i inputs]} (f program i pmodes inputs)]
      (if (= opcode 99)
        outputs
        (recur next-i inputs (concat outputs output))))))

(defn fresh-input []
  (into-array
   (map clojure.edn/read-string
        (clojure.string/split (slurp "input.txt") #","))))

(execute (fresh-input) [1])
;; => (0 0 0 0 0 0 0 0 0 15386262)

(execute (fresh-input) [5])
;; => (10376124)















