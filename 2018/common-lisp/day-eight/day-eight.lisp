(defparameter *input* (uiop:read-file-forms "day-eight.txt"))
;; => (2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2)

(defun take (n list &optional result)
  (cond ((null list) (reverse result))
        ((zerop n) (reverse result))
        (t (take (1- n) (rest list) (cons (first list) result)))))

(defun drop (n list)
  (cond ((null list) nil)
        ((zerop n) list)
        (t (drop (1- n) (rest list)))))

(defun parse-leaf (input meta-count &optional children)
  (values (list (reverse children) (take meta-count input))
          (drop meta-count input)))

(defun parse-2 (input child-count meta-count &optional children)
  (cond ((zerop child-count) (parse-leaf input meta-count children))
        (t (multiple-value-bind (child rest) (parse input)
             (parse-2 rest (1- child-count) meta-count (cons child children))))))

(defun parse (input)
  (parse-2 (rest (rest input)) (first input) (second input)))

(parse *input*)
;; => (((NIL (10 11 12)) (((NIL (99))) (2))) (1 1 2))
;;    NIL

(defparameter *tree* (parse *input*))

(defun children (node)
  (first node))

(children (children *tree*))
;; => (NIL (10 11 12))

(defun meta (node)
  (second node))

(defun sum-metadata (tree)
  (+ (apply #'+ (append (meta tree) (mapcar #'sum-metadata (children tree))))))

(sum-metadata *tree*)
;; => 40977

;; part two

(defun node-value (node)
  (cond
    ((null node) 0)
    ((null (children node)) (apply #'+ (meta node)))
    (t (apply #'+ (mapcar #'node-value (mapcar #'(lambda (index) (nth (1- index) (children node))) (meta node)))))))

(node-value *tree*)
;; => 27490

