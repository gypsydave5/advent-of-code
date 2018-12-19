(defpackage #:day-two
  (:use #:cl))

(in-package #:day-two)

(defparameter *ids* (uiop:read-file-lines "day-two-input.txt"))

;; part one

(defun aget (alist key &key (test #'eql) default)
  (let ((kv (assoc key alist :test test)))
    (if kv
        (cdr kv)
        default)))

(defun to-list (seq)
  (coerce seq 'list))

(defun exactly-n-list (n id &optional (seen nil))
  (cond ((null id) (rassoc n seen))
        (t
         (exactly-n-list n
                         (rest id)
                         (cond ((assoc (first id) seen)
                                (incf (cdr (assoc (first id) seen)))
                                seen)
                               (t (acons (first id) 1 seen)))))))

(mapcar #'(lambda (id) (exactly-n-list 2 id)) (mapcar #'to-list *ids*))

(apply #'*
       (reduce #'(lambda (counts id)
                   (let ((id (coerce id 'list)))
                     (list (+ (first counts) (if (exactly-n-list 2 id) 1 0))
                           (+ (second counts) (if (exactly-n-list 3 id) 1 0)))))
               *ids* :initial-value '(0 0)))

;; part two

(defun differ-by-one (one two &optional (difference 0))
  (cond ((null one) (= difference 1))
        ((< 1 difference) nil)
        ((equal (first one) (first two)) (differ-by-one (rest one) (rest two) difference))
        (t (differ-by-one (rest one) (rest two) (1+ difference)))))

(differ-by-one '(1 2 3) '(1 2 3))
;; => NIL
(differ-by-one '(1 2 3) '(1 2 4))
;; => T
(differ-by-one '(1 2 3) '(1 3 4))
;; => NIL

(defun any-differing (id ids)
  (cond ((null ids) nil)
        ((differ-by-one id (first ids)) (list id (first ids)))
        (t (any-differing id (rest ids)))))

(defun find-differing (ids)
  (cond ((null ids) nil)
        ((not (any-differing (first ids) (rest ids))) (find-differing (rest ids)))
        (t (any-differing (first ids) (rest ids)))))

(defun remove-difference (one two)
  (cond ((null one) nil)
        ((equal (first one) (first two)) (cons (first one) (remove-difference (rest one) (rest two))))
        (t (remove-difference (rest one) (rest two)))))

(defun to-string (seq)
  (coerce seq 'string))

(to-string (apply #'remove-difference (find-differing (mapcar #'to-list *ids*))))
;; => "rmyxgdlihczskunpfijqcebtv"