(ql:quickload "drakma")

(defpackage #:advent-of-code-one
  (:use #:cl #:drakma))

(in-package :advent-of-code-one)

(defparameter *input*  (uiop:read-file-forms "day-one-input.txt"))
;; part 1
(apply #'+ *input*)
;; => 533

;; part 2
(setf *print-circle* t)

(defun make-circular (list)
  (let ((copy (copy-seq list)))
    (setf (cdr (last copy)) copy)
    copy))

(defun first-repeated-frequency (current changes &optional seen)
  (cond ((member current seen) (values current (length seen)))
        (t (first-repeated-frequency (+ current (first changes))
                                     (rest changes)
                                     (cons current seen)))))

(first-repeated-frequency 0 (make-circular *input*))
;; => 73272
;;    130409
(time (first-repeated-frequency 0 (make-circular *input*)))
  ;; Evaluation took:
  ;; 27.138 seconds of real time
  ;; 62.588565 seconds of total run time (62.464294 user, 0.124271 system)
  ;; 230.63% CPU
  ;; 59,568,277,634 processor cycles
  ;; 2,457,120 bytes consed

(defun first-repeated-frequency-hash (current changes &optional (seen (make-hash-table)))
  (cond ((gethash current seen) current)
        (t
         (setf (gethash current seen) t)
         (first-repeated-frequency-hash (+ current (first changes))
                                        (rest changes)
                                        seen))))
(first-repeated-frequency-hash 0 (make-circular *input*))
;; => 73272
(time (first-repeated-frequency-hash 0 (make-circular *input*)))
;; => 73272
(first-repeated-frequency-hash 0 (make-circular *input*))
;; => 73272
  ;; Evaluation took:
  ;; 0.012 seconds of real time
  ;; 0.019580 seconds of total run time (0.017753 user, 0.001827 system)
  ;; 166.67% CPU
  ;; 25,538,576 processor cycles
  ;; 10,437,216 bytes consed
