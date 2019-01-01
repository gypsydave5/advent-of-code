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
