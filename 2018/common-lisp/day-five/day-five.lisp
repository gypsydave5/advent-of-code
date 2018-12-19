(defparameter *polymer*
  (remove #\Newline (coerce (uiop:read-file-string "day-five-input.txt") 'list)))

(defun opposite-polarities? (unit1 unit2)
  (or (and (char= (char-upcase unit1) unit2)
           (char= unit1 (char-downcase unit2)))
      (and (char= unit1 (char-upcase unit2))
           (char= (char-downcase unit1) unit2))))

(opposite-polarities? #\a #\a)
;; => NIL
(opposite-polarities? #\a #\A)
;; => T
(opposite-polarities? #\A #\a)
;; => T
(opposite-polarities? #\A #\b)
;; => NIL
(opposite-polarities? #\A #\A)
;; => NIL

(defun one-reaction (polymer &optional result)
  (cond ((endp polymer)
         result)
        ((endp (cdr polymer))
         (cons (first polymer) result))
        ((opposite-polarities? (first polymer) (second polymer))
         (one-reaction (rest (rest polymer)) result))
        (t
         (one-reaction (rest polymer) (cons (first polymer) result)))))
;; => ONE-REACTION

(one-reaction '(#\a #\a))
;; => (#\a #\a)
(one-reaction '(#\a #\A))
;; => NIL
(one-reaction '(#\C #\a #\A #\b))
;; => (#\C #\b)
(one-reaction (one-reaction '(#\X #\B #\a #\A #\b)))
;; => (#\X)
(one-reaction '(#\A #\A #\a))
;; => (#\A)
(one-reaction '(#\a #\A #\A #\A))
;; => (#\A #\A)

(defun total-reaction (polymer)
  (loop
     for l = (length polymer) then (length p)
     and p = (one-reaction polymer) then (one-reaction p)
     do
       (when (= (length p) l) (return p))))

(total-reaction (coerce "dabAcCaCBAcCcaDA" 'list))
;; => (#\d #\a #\b #\C #\B #\A #\c #\a #\D #\A)

(length *polymer*)
;; => 50000

(length (total-reaction *polymer*))
;; => 11310

;; part two

(defun strip-type (type polymer)
  (remove type polymer :test #'equalp))

(defun all-chars ()
  (loop for c from (char-code #\a) to (char-code #\z) collect (code-char c)))
;; => ALL-CHARS
(all-chars)
;; =>
;; (#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s
;;  #\t #\u #\v #\w #\x #\y #\z)

(defun optimal-removal (polymer)
  (loop for unit in (all-chars)
     minimize (length (total-reaction (strip-type unit polymer)))))

(optimal-removal (total-reaction *polymer*))
;; => 6020
