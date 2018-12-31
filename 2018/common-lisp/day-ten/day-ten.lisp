(ql:quickload "str")

(defparameter *line* (first (uiop:read-file-lines "day-ten-input.txt")))
;; => "position=< -9951, -50547> velocity=< 1,  5>"

(defun parse-line (line)
  (list (mapcar #'parse-integer (list (subseq line 10 16)
                                      (subseq line 17 24)))
        (mapcar #'parse-integer (list (subseq line 36 38)
                                      (subseq line 39 42)))))

(parse-line *line*)
;; => ((-9951 -50547) (1 5))

(defparameter *input* (mapcar #'parse-line (uiop:read-file-lines "day-ten-input.txt")))
;; => *INPUT*

(defun next-positions (inputs)
  (mapcar #'(lambda (pair) (list (apply #'mapcar #'+ pair)
                                 (second pair)))
          inputs))

;; Premise: the time at which all the points are closest together is the time at which the message is readable
;; Premise: the maximum closeness of all points may be determined by the time when the area needed to display the points is at a minimum

(defun boundaries (inputs)
  (loop for input in inputs
     for position = (first input) then (first input)
     for max-x = (first position) then (max max-x (first position))
     for min-x = (first position) then (min min-x (first position))
     for max-y = (second position) then (max max-y (second position))
     for min-y = (second position) then (min min-y (second position))
     finally (return (list (list max-x min-x) (list max-y min-y)))))

(defun area (bounds)
  (* (apply #'- (first bounds))
     (apply #'- (second bounds))))

(area '((5 1) (5 1)))

(loop
   for i from 0
   with inputs = *input*
   with area = (area (boundaries inputs))
   do
     (cond ((< area (area (boundaries inputs))) (return (values i area)))
           (t
            (setf area (area (boundaries inputs)))
            (setf inputs (next-positions inputs)))))
;; => 10137
;;    30825

;; so it looks like the 10137th iteration is a winner (maybe)... but it's still pretty big!

(defun after-x-seconds (seconds)
  (loop for i from 0 to seconds
     for input = *input* then (next-positions input)
     finally (return input)))

(after-x-seconds 10137)

(ql:quickload "skippy")

(defun draw-stars (input)
  (destructuring-bind ((max-x min-x) (max-y min-y)) (boundaries input)
    (let*
        ((positions (mapcar #'first input))
         (n-positions (normalize-positions min-x min-y positions)))
      (loop for y from 0 to (- max-y min-y) collect
           (str:join "" (loop for x from 0 to (- max-x max-y) collect
                          (if (member (list x y) n-positions :test #'equal)
                              "x"
                              " ")))))))

(defun normalize-positions (min-x min-y positions)
  (mapcar #'(lambda (position) (list (- (first position) min-x)
                                     (- (second position) min-y)))
          positions))

;; off by one!
(draw-stars (after-x-seconds 10136))

;; part two

;; um... looks like I alread have it :D
