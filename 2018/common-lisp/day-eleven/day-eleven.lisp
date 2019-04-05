(defparameter *grid-serial-number* 5535)

(defun extract-hundreds (n)
  (multiple-value-bind (_ hundreds) (floor (/ (floor (/ n 100)) 10))
    (* hundreds 10)))

(extract-hundreds 10108537)

(defun power-level (coordinate)
  (destructuring-bind (x y) coordinate
    (let* ((rack-id (+ 10 x)))
      (- (extract-hundreds (* rack-id (+ *grid-serial-number* (* rack-id y))))
         5))))

(let ((*grid-serial-number* 8))
  (power-level '(3 5)))
;; => 4

(let ((*grid-serial-number* 39))
  (power-level '(217 196)))
;; => 0

(let ((*grid-serial-number* 57))
  (power-level '(122 79)))
;; => -5

(let ((*grid-serial-number* 71))
  (power-level '(101 153)))
;; => 4

(defun to-3x3 (coordinate)
  (mapcar #'(lambda (c) (mapcar #'+ c coordinate))
          '((0 0) (0 1) (0 2)
            (1 0) (1 1) (1 2)
            (2 0) (2 1) (2 2))))

(defun total-power-3x3 (coordinate)
  (apply #'+ (mapcar #'power-level (to-3x3 coordinate))))

(let ((*grid-serial-number* 18))
  (total-power-3x3 '(33 45)))
;; => 29

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))

(most #'second (loop for coordinate in
                    (loop for x from 1 to 298 nconc
                         (loop for y from 1 to 298 collect
                              (list x y)))
                  collect (list coordinate (total-power-3x3 coordinate))))

;; => ((19 41) 29)
;;    29

;; part two
(defparameter *square-size* 300)
(defparameter *power-grid* (let ((a (make-array '((1+ *square-size*)
                                                  (1+ *square-size*)))))
                             (loop for x from 1 to *square-size* do
                                  (loop for y from 1 to *square-size* do
                                       (setf )))))

(defun to-square (coordinate size)
  (mapcar #'(lambda (c) (mapcar #'+ c coordinate))
          (loop for x from 0 to (1- size) nconc
               (loop for y from 0 to (1- size) collect
                    (list x y)))))

(to-square '(1 1) 2)
;; => ((1 1) (1 2) (2 1) (2 2))

(defun col-and-row-out (coordinate size)
  (let ((size (1- size)))
    (nconc (loop for x from 0 to size collect
                (list (+ (first coordinate) x) (+ size (second coordinate))))
           (loop for y from 0 to (1- size) collect
                (list (+ (first coordinate) size) (+ (second coordinate) y))))))

(col-and-row-out '(1 1) 4)
;; => ((1 4) (2 4) (3 4) (4 4) (4 1) (4 2) (4 3))

(defun all-powers (origin)
  (let ((squares (list (list 1 (power-level origin))))
        (max-size (1+ (min (- *square-size* (first origin))
                           (- *square-size* (second origin))))))
    (loop for size from 2 to max-size
       do
         (push (list size (apply #'+ (second (first squares))
                             (mapcar #'power-level (col-and-row-out origin size))))
               squares))
    squares))

(all-powers '(288 288))
;; =>
;; ((13 -36) (12 -47) (11 -48) (10 -22) (9 -10) (8 -18) (7 -10) (6 -22) (5 -5)
;;  (4 4) (3 5) (2 8) (1 4))

(defun max-power (origin)
  (most #'second (all-powers origin)))
;; => MAX-POWER

(max-power '(288 288))
;; => (2 8)
;;    8

(defun total-powers (coordinate)
  (let ((max-possible-square (1+ (min (- *square-size* (first coordinate))
                                      (- *square-size* (second coordinate))))))
    (loop for size from 1 to max-possible-square
       collect (list size (apply #'+ (mapcar #'power-level (to-square coordinate size)))))))

(let ((*square-size* 300))
  (most #'cadadr (loop for coordinate in
                      (loop for x from 1 to *square-size* nconc
                           (loop for y from 1 to *square-size* collect
                                (list x y)))
                    collect (list coordinate (max-power coordinate)))))


