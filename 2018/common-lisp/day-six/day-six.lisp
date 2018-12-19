(ql:quickload "str")
(defparameter *coordinates* (mapcar #'parse-coordinate (uiop:read-file-lines "day-six-input.txt")))

(defun parse-coordinate (coord)
  (mapcar #'parse-integer (str:split ", " coord)))

(defun min-max (list &key (key #'identity))
  (loop for i in list
     maximize (funcall key i) into max
     minimize (funcall key i) into min
     finally (return (list min max))))

(defun x (coord)
  (first coord))

(defun y (coord)
  (second coord))

(defun bounds (coordinates)
  (list (min-max coordinates :key #'x)
        (min-max coordinates :key #'y)))

(bounds *coordinates*)
;; => ((42 357) (40 357))

(defun bounded-coordinates (min-max)
  (destructuring-bind ((min-x max-x) (min-y max-y)) min-max
      (loop for y from min-y to max-y append
           (loop for x from min-x to max-x collect
                (list x y)))))
;; => BOUNDED-COORDINATES

(bounded-coordinates (bounds *coordinates*))

(defun distance (coordinate-1 coordinate-2)
  (+ (abs (- (x coordinate-1) (x coordinate-2)))
     (abs (- (y coordinate-1) (y coordinate-2)))))

(distance '(1 1) '(3 3))
;; => 4
(distance '(3 4) '(4 4))
;; => 1
(distance '(5 5) '(5 5))
;; => 0
(distance '(8 3) '(5 5))
;; => 5

(defun closest-point (coordinate points)
  (loop for point in points
     with min
     with min-points = nil
     do
       (cond ((null min)
              (push point min-points)
              (setf min (distance point coordinate)))
             ((< (distance point coordinate) min)
              (setf min-points (list point))
              (setf min (distance point coordinate)))
             ((= (distance point coordinate) min)
              (push point min-points)))
     finally (return (when (= 1 (length min-points))
                       (values (first min-points) min)))))

(defun sorted-points-by-proximity (coordinate points)
  (sort (mapcar #'(lambda (p) (cons coordinate (list p (distance p coordinate)))) points)
        #'< :key #'third))

(closest-point '(5 5) *coordinates*)
;; => (5 5)
;;    0

(defun closest-points (coordinates points)
  (mapcar
   #'(lambda (coord) (append coord (list (closest-point coord points))))
   coordinates))

(closest-points (bounded-coordinates (bounds *coordinates*)) *coordinates*)
;; =>
;; ((1 1 (1 1)) (2 1 (1 1)) (3 1 (1 1)) (4 1 (1 1)) (5 1 NIL) (6 1 (8 3))
;;  (7 1 (8 3)) (8 1 (8 3)) (1 2 (1 1)) (2 2 (1 1)) (3 2 (3 4)) (4 2 (3 4))
;;  (5 2 (5 5)) (6 2 (8 3)) (7 2 (8 3)) (8 2 (8 3)) (1 3 (1 1)) (2 3 (3 4))
;;  (3 3 (3 4)) (4 3 (3 4)) (5 3 (5 5)) (6 3 (8 3)) (7 3 (8 3)) (8 3 (8 3))
;;  (1 4 NIL) (2 4 (3 4)) (3 4 (3 4)) (4 4 (3 4)) (5 4 (5 5)) (6 4 (5 5))
;;  (7 4 (8 3)) (8 4 (8 3)) (1 5 (1 6)) (2 5 NIL) (3 5 (3 4)) (4 5 (5 5))
;;  (5 5 (5 5)) (6 5 (5 5)) (7 5 (5 5)) (8 5 (8 3)) (1 6 (1 6)) (2 6 (1 6))
;;  (3 6 NIL) (4 6 (5 5)) (5 6 (5 5)) (6 6 (5 5)) (7 6 (5 5)) (8 6 NIL)
;;  (1 7 (1 6)) (2 7 (1 6)) (3 7 NIL) (4 7 (5 5)) (5 7 (5 5)) (6 7 (5 5))
;;  (7 7 (8 9)) (8 7 (8 9)) (1 8 (1 6)) (2 8 (1 6)) (3 8 NIL) (4 8 (5 5))
;;  (5 8 (5 5)) (6 8 (8 9)) (7 8 (8 9)) (8 8 (8 9)) (1 9 (1 6)) (2 9 (1 6))
;;  (3 9 NIL) (4 9 (8 9)) (5 9 (8 9)) (6 9 (8 9)) (7 9 (8 9)) (8 9 (8 9)))

(defun owner (coord)
  (third coord))

(defun infinities (owned-coords bounds)
  (destructuring-bind ((min-x max-x) (min-y max-y)) bounds
    (remove-duplicates (loop for coord in owned-coords
                          when (or (= (x coord) min-x)
                                   (= (x coord) max-x)
                                   (= (y coord) min-y)
                                   (= (y coord) max-y))
                          collect (owner coord)))))

(infinities (closest-points (bounded-coordinates (bounds *coordinates*)) *coordinates*)
            (bounds *coordinates*))

(defun remove-coords-with-owners (coords owners)
  (loop for coord in coords
     unless (member (owner coord) owners)
     collect coord))

(remove-infinities
 (closest-points (bounded-coordinates (bounds *coordinates*)) *coordinates*)
 (infinities (closest-points (bounded-coordinates (bounds *coordinates*)) *coordinates*)
             (bounds *coordinates*)))

(defun mode (xs)
  "Calculates the modes of XS and returns them as well as the count."
  (let ((counts (make-hash-table :test 'equal))
        (max 0)
        (modes ()))
    (dolist (x xs)
      (multiple-value-bind (count present) (gethash x counts)
        (if present
            (setf (gethash x counts) (1+ count))
            (setf (gethash x counts) 1))))
    (maphash #'(lambda (value count)
                 (cond ((= count max) (push value modes))
                       ((> count max)
                        (setf max count
                              modes (list value))))) counts)
    (values modes max)))

(mode (mapcar #'owner (remove-infinities
                       (closest-points (bounded-coordinates (bounds *coordinates*)) *coordinates*)
                       (infinities (closest-points (bounded-coordinates (bounds *coordinates*))
                                                   *coordinates*)
                                   (bounds *coordinates*)))))

(defun find-point-with-most-locations (points)
  (let* ((bounds (bounds points))
         (all-coordinates (bounded-coordinates bounds))
         (coords-with-owners (closest-points all-coordinates points))
         (infinities (infinities coords-with-owners bounds)))
    (mode (mapcar #'owner (remove-coords-with-owners coords-with-owners infinities)))))

(find-point-with-most-locations *coordinates*)
;; => ((277 126))
;;    2342

;; part two

(defun total-distance-to-all-points (coordinate points)
  (apply #'+ (mapcar #'(lambda (point) (distance point coordinate)) points)))

(defun coords-with-total-distances (coords points)
  (mapcar #'(lambda (coord) (list coord (total-distance-to-all-points coord points)))
          coords))

(defun td (coord)
  (second coord))

(defun locations-with-total-distance (points)
  (let* ((bounds (bounds points))
         (all-coordinates (bounded-coordinates bounds))
         (coords-with-owners (closest-points all-coordinates points))
         (infinities (infinities coords-with-owners bounds)))
    (remove-if #'(lambda (coord) (>= (td coord) 10000))
               (coords-with-total-distances all-coordinates points))))

(length (locations-with-total-distance *coordinates*))
;; => 43302
