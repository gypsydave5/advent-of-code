(ql:quickload "str")
(ql:quickload "cl-date-time-parser")
(ql:quickload "local-time")

(defparameter *raw-logs* (uiop:read-file-lines "day-four-input.txt"))
(defparameter *test-raw-log* (first *raw-logs*))

(defun parse-timestamp (raw-log)
  (cl-date-time-parser:parse-date-time (subseq raw-log 7 17)))

(parse-timestamp *test-raw-log*)
;; => 3746216880
;;    0

(defun parse-message (raw-log)
  (let ((message-text (subseq raw-log 19)))
    (ecase (char message-text 0)
      (#\G (list 'guard (get-guard-id message-text)))
      (#\f (list 'fall 'asleep))
      (#\w (list 'wake 'up)))))

(defun get-guard-id (message-text)
  (parse-integer (second (str:split "#" message-text)) :junk-allowed t))

(parse-message *test-raw-log*)
;; => (GUARD 1307)

(defparameter *fall-asleep-log* (third *raw-logs*))
(parse-message *fall-asleep-log*)
;; => (FALL ASLEEP)

(defparameter *wake-up-log* (sixth *raw-logs*))
(parse-message *wake-up-log*)
;; => (WAKE UP)

(defun parse-log (raw-log)
  (list (parse-timestamp raw-log)
        (parse-message raw-log)))
(parse-log *test-raw-log*)
;; => (3746216880 (GUARD 1307))

(defparameter *logs* (mapcar #'parse-log *raw-logs*))
(defparameter *sorted-logs* (sort *logs* #'< :key #'first))

(defun timestamp->minute (timestamp)
  (multiple-value-bind (_ minute)  (Decode-universal-time timestamp)
    minute))

(timestamp->minute (first (second *sorted-logs*)))
;; => 42

(defun sleep-map (logs)
  (let ((map (make-hash-table))
        (current-guard-id))
    (loop for log in logs do
         (ecase (first (second log))
           (guard (setf current-guard-id (second (second log))))
           (fall (push (timestamp->minute (first log)) (gethash current-guard-id map)))
           (wake (push (timestamp->minute (first log)) (gethash current-guard-id map)))))
    map))

(defun total-sleep-duration (logs)
  (let ((map (sleep-map logs)))
    (loop for guard being each hash-key in map collect
         (list guard (loop for period in (sleep-periods guard map)
                    sum (period->minutes period))))))

(total-sleep-duration *sorted-logs*)
;; =>
;; ((947 466) (557 366) (3271 400) (113 259) (3067 279) (1811 321) (389 450)
;;  (449 367) (601 128) (1237 146) (739 187) (1307 380) (3217 467) (2647 332)
;;  (3209 541) (1559 406) (1459 432) (877 378) (409 160) (1033 227))

(defun longest-sleeper (logs)
  (reduce #'(lambda (g1 g2)
              (if (> (second g1) (second g2)) g1 g2))
          (total-sleep-duration logs)))

(longest-sleeper *sorted-logs*)
;; => (3209 541)

(defun partition (list)
  (cond ((null list) nil)
        ((null (rest list)) (list (list (first list))))
        (t (cons (list (first list) (second list))
                 (partition (rest (rest list)))))))

(defun sleep-periods (guard-id sleep-map)
  (mapcar #'reverse (partition (gethash guard-id sleep-map))))

(defun start (period)
  (car period))

(defun end (period)
  (cdr period))

(defun period->minutes (period)
  (- (second period) (first period)))

(defun sleep-period->minutes (period)
  (loop for n from (first period) to (1- (second period)) collect n))

(defun most-sleepful-minutes (periods)
  (mode (apply #'append (mapcar #'sleep-period->minutes
                                periods))))

(defun most-sleepful-minute-of-sleepiest-guard (logs)
  (let* ((sleepiest-guard (first (longest-sleeper logs)))
         (map (sleep-map logs)))
    (list sleepiest-guard (first (most-sleepful-minutes
                                  (sleep-periods sleepiest-guard map))))))

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

(most-sleepful-minute-of-sleepiest-guard *sorted-logs*)
;; => (3209 32)

(* 3209 32)
;; => 102688

;; part two
(defun most-sleepful-minute-of-any-guard (logs)
  (let ((map (sleep-map logs)))
    (loop for guard being each hash-key in map
       with max-frequency = 0
       with sleepiest-minute
       with sleepiest-guard
       do
         (multiple-value-bind (minutes frequency) (most-sleepful-minutes
                                                   (sleep-periods guard map))
           (when (> frequency max-frequency)
             (setq sleepiest-minute (first minutes)
                   sleepiest-guard guard
                   max-frequency frequency)))
       finally
         (return (values (list sleepiest-guard sleepiest-minute) max-frequency)))))

(most-sleepful-minute-of-any-guard *sorted-logs*)
;; => (1459 39)
;;    18

(apply #'* (most-sleepful-minute-of-any-guard *sorted-logs*))
;; => 56901
