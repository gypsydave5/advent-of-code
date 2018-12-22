(ql:quickload "str")

(defun pick (list &rest indexes)
  (mapcar #'(lambda (index) (nth index list)) indexes))

(defparameter *input*
  (mapcar #'parse-integer
          (pick (str:words (uiop:read-file-line "day-nine-input.txt"))
                0 6)))

(defun create-marbles (top-score)
  (loop for i from 1 to top-score collect i))

(defun create-players (total-players)
  (loop for i from 1 to total-players collect (list i 0)))

(defun rotate-cw (list count)
  (cond ((zerop count) list)
        (t (rotate-cw (append (rest list) (list (first list))) (1- count)))))

(defun rotate-ccw (list count)
  (cond ((zerop count) list)
        (t (rotate-ccw (append (last list) (butlast list)) (1- count)))))

(rotate-cw '(1 2 3 4 5 6) 2)
;; => (3 4 5 6 1 2)

(rotate-ccw '(1 2 3 4 5 6) 2)
;; => (5 6 1 2 3 4)

(defparameter *initial-circle* '(0))

(defun add-marble (new-marble circle)
  (cond (t (cons new-marble (rotate-cw circle 2)))))

(add-marble 2 (add-marble 1 *initial-circle*))
;; => (2 1 0)

(defun marble-winner (player-count marble-top-score)
  (let ((players (create-players player-count))
        (marbles (create-marbles marble-top-score)))
    (marble-run marbles players *initial-circle*)))

(defun next-player (players &optional (score 0))
  (append (rest players) (list (list (first (first players)) (+ score (second (first players)))))))

(next-player (create-players 5))
;; => ((2 0) (3 0) (4 0) (5 0) (1 0))

(next-player (create-players 5) 1337)
;; => ((2 0) (3 0) (4 0) (5 0) (1 1337))

(defun marble-run (marbles players circle)
  (cond ((null marbles) players)
        ((= (mod (first marbles) 23) 0)
         (let ((seven-back (rotate-ccw circle 7)))
           (marble-run (rest marbles)
                       (next-player players (+ (first marbles)
                                               (first seven-back)))
                       (rest seven-back))))
        (t (marble-run (rest marbles)
                       (next-player players)
                       (add-marble (first marbles) circle)))))

(second (first (sort (marble-winner 10 1618) #'> :key #'second)))

(second (first (sort (marble-winner (first *input*) (second *input*)) #'> :key #'second)))
;; => 398371

(defun winning-score (players marbles)
  (second (first (sort (marble-winner players marbles) #'> :key #'second))))

;; part two

(time (winning-score 10 16180))
;; => 756228

(defun rotate-cw (list count)
  (if (= 0 count) list
      (let ((firsts)
            (rests))
        (setf firsts (loop for x from 1 to count
                        for e on list
                        collect (first e)
                        finally (setf rests (rest e))))
        (nconc rests firsts))))

(rotate-cw '(1 2 3 4 5 6 7 8 9) 3)
;; => (4 5 6 7 8 9 1 2 3)

(defun rotate-ccw (list count)
  (reverse (rotate-cw (reverse list) count)))

(rotate-ccw '(1 2 3 4 5 6 7 8 9) 3)
;; => (7 8 9 1 2 3 4 5 6)

(defun marble-run (marbles players circle)
  (cond ((null marbles) players)
        ((= (mod (first marbles) 23) 0)
         (let ((seven-back (rotate-ccw circle 7)))
           (marble-run (rest marbles)
                       (next-player players (+ (first marbles)
                                               (first seven-back)))
                       (rest seven-back))))
        (t (marble-run (rest marbles)
                       (next-player players)
                       (add-marble (first marbles) circle)))))


(winning-score (first *input*) (* 100 (second *input*)))
;; => 398371
