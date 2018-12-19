(ql:quickload "cl-ppcre")
(ql:quickload "str")

(defparameter *raw-claims* (remove "" (uiop:read-file-lines "day-three-input.txt") :test #'string-equal))
(defparameter *test-claim* (first *raw-claims*))

(defun to-list (seq)
  (coerce seq 'list))

(to-list *test-claim*)

(defun read-id (claim)
  (parse-integer (subseq (first (str:words claim)) 1)))

(read-id *test-claim*)
;; => 1
;;    1

(defun read-coords (claim)
  (mapcar #'parse-integer (str:split "," (first (str:split ":" (third (str:words claim)))))))

(read-coords *test-claim*)
;; => (469 741)

(defun read-dimensions (claim)
  (mapcar #'parse-integer (str:split "x" (fourth (str:words claim)))))

(read-dimensions *test-claim*)
;; => (22 26)

(defun parse-claim (raw-claim)
  (list (read-id raw-claim)
        (read-coords raw-claim)
        (read-dimensions raw-claim)))

(defparameter *claims* (mapcar #'parse-claim *raw-claims*))

(defun origin-x (claim)
  (first (second claim)))
(defun origin-y (claim)
  (second (second claim)))
(defun dimension-x (claim)
  (first (third claim)))
(defun dimension-y (claim)
  (second (third claim)))

(defun claim->coordinates (claim)
  (coordinates (origin-x claim) (origin-y claim) (dimension-x claim) (dimension-y claim)))

(defun coordinates (origin-x origin-y dimension-x dimension-y)
  (cond ((zerop dimension-x) nil)
        (t (append (coordinates (1+ origin-x) origin-y (1- dimension-x) dimension-y)
                   (row origin-x origin-y dimension-x dimension-y)))))

(defun row (origin-x origin-y dimension-x dimension-y)
  (cond ((zerop dimension-y) nil)
        (t (cons (list origin-x origin-y)
                 (row origin-x (1+ origin-y) dimension-x (1- dimension-y))))))

(length (claim->coordinates (parse-claim *test-claim*)
                            ;; => (1 (469 741) (22 26))
                            ))
;; => 572

(* 22 26)
;; => 572

(defun accumulate-coords (claims &optional (array (make-array '(1000 1000) :initial-element 0 :element-type 'integer)))
  (cond ((null claims) array)
        (t (accumulate-coords (rest claims) (add-coords (claim->coordinates (first claims)) array)))))

(defun add-coords (coords array)
  (let ((current (when coords (aref array (first (first coords)) (second (first coords))))))
    (cond ((not current) array)
          (t
           (incf (aref array (first (first coords)) (second (first coords))))
           (add-coords (rest coords) array)))))

(defun claim-inch (array x y)
  (incf (aref array x y))
  array)

(claim-inch (make-array '(4 4) :initial-element 0) 2 2)

(add-coords (list (list 1 1)) (make-array '(4 4) :initial-element 0))

(defun overlaps (claims)
  (let ((cloth (accumulate-coords claims)))
    (destructuring-bind (width height) (array-dimensions cloth)
      (loop for x from 0 below width append
           (loop for y from 0 below height 
              when (< 1 (aref cloth x y))
              collect (list x y))))))

(length (overlaps *claims*))
;; => 96569

;; part two

(defun find-no-overlaps (claims overlaps)
  (cond ((null claims) nil)
        ((no-overlap (claim->coordinates (first claims)) overlaps)
             (first (first claims)))
        (t (find-no-overlaps (rest claims) overlaps))))

(defun no-overlap (coords overlaps)
  (cond ((null coords) t)
        ((not (= 1 (aref overlaps (first (first coords)) (second (first coords))))) nil)
        (t (no-overlap (rest coords) overlaps))))

(find-no-overlaps *claims* (accumulate-coords *claims*))
;; => 1023
