(ql:quickload "str")

(defparameter *input*
  (uiop:read-file-lines "day-seven-input.txt"))

(defun parse-instruction (instruction)
  (let ((parsed (str:words instruction)))
    (list (intern (second parsed))
          (intern (eighth parsed)))))

(defparameter *nodes* (mapcar #'parse-instruction *input*))

(defparameter *node-names* (remove-duplicates (reduce #'append *nodes*)))

(defun alphabetize-symbol-list (list)
  (sort list #'string< :key #'symbol-name))

(defun find-roots (nodes)
  (set-difference *node-names* (mapcar #'second nodes)))

(find-roots *nodes*)
;; => (Z X V O)

(defun unblock (node-name nodes)
  (remove-if #'(lambda (node) (eq (first node) node-name)) nodes))

(alphabetize-symbol-list (find-roots (unblock 'o *nodes*)))
;; => (O V X Z)

(defun options (nodes completed)
  (alphabetize-symbol-list (set-difference (find-roots nodes) completed)))

(defun do-it (nodes &optional completed)
  (let ((options (options nodes completed)))
    (cond ((null options) (reverse completed))
          (t (do-it (unblock (first options) nodes) (cons (first options) completed))))))

(princ (str:join "" (mapcar #'symbol-name (do-it *nodes*))))
;; => OVXCKZBDEHINPFSTJLUYRWGAMQ
;;
;;    "OVXCKZBDEHINPFSTJLUYRWGAMQ"

;; part two

(defun time-for-step (step)
  (- (char-code (first (coerce (string step) 'list))) 4))

(defun elf-time (elf)
  (second elf))

(defun elf-step (elf)
  (first elf))

(defun shortest-working-elf (elves)
  (first (remove-if #'(lambda (elf) (null (elf-step elf)))
                    (sort elves #'< :key #'elf-time))))

(shortest-working-elf '((a 10) (b 20) (c 5)))
;; => (C 5)

(shortest-working-elf '((a 10) (b 20) (c 5) (nil 0)))
;; => (C 5)

(defun work-for (elf time)
  (list (elf-step elf) (max 0 (- (elf-time elf) time))))

(defun elves-work-for (elves time)
  (mapcar #'(lambda (elf) (work-for elf time)) elves))

(defun new-elf (step)
  (list step (time-for-step step)))

(defun completed-steps (elves)
  (alphabetize-symbol-list
   (mapcar #'elf-step
           (remove-if #'null (remove-if-not #'(lambda (elf) (zerop (elf-time elf))) elves)
                      :key #'elf-step))))

(defun workless-elf? (elf)
  (or (null (elf-step elf))
      (zerop (elf-time elf))))

(defun in-progress (elves)
  (mapcar #'elf-step (remove-if #'workless-elf? elves)))

(completed-steps (next-elves '((a 10) (b 20) (c 5))))
;; => (C)

(defun set-elves-to-work (elves options &optional new-elves)
  (cond ((null elves) new-elves)
        ((null options) (append elves new-elves))
        ((workless-elf? (first elves)) (set-elves-to-work
                                        (rest elves)
                                        (rest options)
                                        (cons (list (first options)
                                                    (time-for-step (first options)))
                                              new-elves)))
        (t (set-elves-to-work
            (rest elves)
            options
            (cons (first elves) new-elves)))))

(defun initial-elves ()
  '((nil 0) (nil 0) (nil 0) (nil 0) (nil 0)))

(set-elves-to-work (initial-elves) '(a b c d e f g h i j))
;; => ((E 65) (D 64) (C 63) (B 62) (A 61))

(defun unblock-all (steps nodes)
  (remove-if #'(lambda (node) (member (first node) steps)) nodes))

(defun reset-elves (elves)
  (mapcar #'(lambda (elf) (if (zerop (elf-time elf))
                              (list nil 0)
                              elf))
          elves))

(defun all-elves-finished? (elves)
  (every #'workless-elf? elves))

(defun how-long (nodes &optional completed (elves (initial-elves)) (total-time 0))
  (let* ((next-steps (options nodes (append (in-progress elves) completed)))
         (working-elves (set-elves-to-work elves next-steps))
         (time-passed (or (elf-time (shortest-elf elves)) 0))
         (new-elves (elves-work-for working-elves time-passed)))
    (cond
      ((and (null next-steps) (all-elves-finished? elves)) total-time)
      (t (how-long (unblock-all(append completed (completed-steps new-elves)) nodes)
                   (append completed (completed-steps new-elves))
                   (reset-elves new-elves)
                   (+ total-time time-passed))))))

(how-long *nodes*)
;; => 955
