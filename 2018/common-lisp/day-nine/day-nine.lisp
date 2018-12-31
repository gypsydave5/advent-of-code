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
  (append (rest players)
          (list (list (first (first players))
                      (+ score (second (first players)))))))

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

(defun make-circle (list)
  (list (length list) (nconc list list)))

(make-circle (list 1 2 3))
;; => (3 #1=(1 2 3 . #1#))

(defun circle-length (circle)
  (first circle))

(defun rotate-circle (circle n)
  (let ((moves (if (minusp n) (+ (circle-length circle) n) n)))
    (dotimes (_ moves)
      (setf (second circle) (cdr (second circle))))
    circle))

(rotate-circle (make-circle '(1 2 3)) 1)
;; => (3 #1=(2 3 1 . #1#))
(rotate-circle (make-circle '(1 2 3)) -1)
;; => (3 #1=(3 1 2 . #1#))

(defun insert-after (circle item)
  (let ((new-cons (list item)))
    (rotatef
     (cdr new-cons)
     (cdr (second circle))
     new-cons)
    
    (list (1+ (circle-length circle))
          (second circle))))

(insert-after (make-circle '(1 2 3)) 4)
;; => (4 #1=(1 4 2 3 . #1#))

(defun current-item (circle)
  (first (second circle)))

(defun remove-next (circle)
  (setf (cdr (second circle))
        (cddr (second circle)))
  (list (1- (circle-length circle))
        (second circle)))

(remove-next (make-circle '(1 2 3 4)))
;; => (3 #1=(1 3 4 . #1#))

(defun add-marble (circle marble)
  (rotate-circle
   (insert-after (rotate-circle circle 1) marble)
   1))

(add-marble (add-marble (add-marble (make-circle (list 0)) 1) 2) 3)
;; => (4 #1=(3 0 2 1 . #1#))

(defun remove-7 (circle)
  (let* ((c (rotate-circle circle -8))
         (removed (second (second circle))))
    (values removed (rotate-circle (remove-next c) 1))))

(remove-7 (make-circle '(0 1 2 3 4 5 6 7 8 9 10)))

(defun break-circle (circle)
  (let ((c (second circle))
        (len (first circle))
        (list))
    (dotimes (_ len)
      (push (first c) list)
      (setf c (cdr c)))
    (reverse list)))

(break-circle (rotate-circle (make-circle '(1 2 3 4)) 2))
;; => (3 4 1 2)

(defun marble-run (marbles players circle)
  (cond ((null marbles) (break-circle players))
        ((= (mod (first marbles) 23) 0)
         (multiple-value-bind (score new-circle) (remove-7 circle)
           (marble-run (rest marbles)
                       (next-players players (+ (first marbles) score))
                       new-circle)))
        (t (marble-run (rest marbles)
                       (next-players players)
                       (add-marble circle (first marbles))))))

(defun create-players (player-count)
  (make-circle (loop for player from 1 to player-count collect (list player 0))))

(defparameter *initial-circle*
  (make-circle (list 0)))

(defun marble-winner (player-count marble-top-score)
  (let ((players (create-players player-count))
        (marbles (create-marbles marble-top-score)))
    (marble-run marbles players (make-circle (list 0)))))

(defun winning-score (players marbles)
  (second (first (sort (marble-winner players marbles) #'> :key #'second))))

(defun next-players (players &optional (score  0))
  (setf (first (second players))
        (list (first (first (second players)))
              (+ score (second (first (second players))))))
  (list (first players) (cdr (second players))))

(next-players (create-players 5) 3)
;; => (5 #1=((2 0) (3 0) (4 0) (5 0) (1 3) . #1#))

(winning-score (first *input*) (* 100 (second *input*)))
;; => 3212830280
