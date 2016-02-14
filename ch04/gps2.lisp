(in-package :gps)

(defvar *ops* nil "A list of available operators.")

(defstruct op "An operation"
           (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun executing-p (x)
  "Is x of the form: (executing ...)?"
  (starts-with x 'executing))
(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))
(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)
(defun op (action &key preconds add-list del-list)
  "Make a new opeator that obeys the (EXECUTING op) convention."
  (convert-op
   (make-op :action action :preconds preconds
            :add-list add-list :del-list del-list)))

(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver: from state, achieve goals using *ops*."
  (find-all-if #'action-p
               (achieve-all (cons '(start) state) goals nil)))

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defun action-p (x)
  "Is x something that is (start) or (executing ...)?"
  (or (equal x '(start)) (executing-p x)))

(defun achieve-all (state goals goal-stack)
  "Achieve each goal, trying serveral orderings."
  (some #'(lambda (goals) (achieve-each state goals goal-stack))
        (orderings goals)))

(defun achieve-each (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

(defun orderings (l)
  (if (> (length l) 1)
      (list l (reverse l))
      (list l)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds,
or if there is an appropriate op for it this is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (appropriate-ops goal state)))))

(defun appropriate-ops (goal state)
  "Return a list of appropriate operators, sorted by the number of
  unfulfilled preconditions."
  (sort (copy-list (find-all goal *ops* :test #'appropriate-p))
        #'< :key #'(lambda (op)
                     (count-if
                      #'(lambda (precond) (not (member-equal precond state)))
                      (op-preconds op)))))

(defun member-equal (item list)
  (member item list :test #'equal))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member-equal goal (op-add-list op)))

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable."
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op)
                             (cons goal goal-stack))))
    (unless (null state2)
      ;; return an updated state
      (dbg-indent :gps (length goal-stack) "Action ~a" (op-action op))
      (append (remove-if #'(lambda (x)
                             (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))

(defun use (oplist)
  "Use oplist as the default list of operators."
  ;; return something useful, but not too verbose:
  ;; the number of operators
  (length (setf *ops* oplist)))

(defparameter *school-ops*
  (list
   (make-op :action 'drive-son-to-school
            :preconds '(son-at-home car-works)
            :add-list '(son-at-school)
            :del-list '(son-at-home))
   (make-op :action 'shop-installs-battery
            :preconds '(car-needs-battery shop-knows-problem shop-has-money)
            :add-list '(car-works))
   (make-op :action 'tell-shop-problem
            :preconds '(in-communication-with-shop)
            :add-list '(shop-knows-problem))
   (make-op :action 'telephone-shop
            :preconds '(know-phone-number)
            :add-list '(in-communication-with-shop))
   (make-op :action 'look-up-number
            :preconds '(have-phone-book)
            :add-list '(know-phone-number))
   (make-op :action 'give-shop-money
            :preconds '(have-money)
            :add-list '(shop-has-money)
            :del-list '(have-money))))

(push (make-op :action 'ask-phone-number
               :preconds '(in-communication-with-shop)
               :add-list '(know-phone-number))
      *school-ops*)
;; ======================== bananas

(defparameter *banana-ops*
  (list
   (op 'climb-on-chair
       :preconds '(chair-at-middle-room at-middle-room on-floor)
       :add-list '(at-bananas on-chair)
       :del-list '(at-middle-room on-floor))
   (op 'push-chair-from-door-to-middle-room
       :preconds '(chair-at-door at-door)
       :add-list '(chair-at-middle-room at-middle-room)
       :del-list '(chair-at-door at-door))
   (op 'walk-from-door-to-middle-room
       :preconds '(at-door on-floor)
       :add-list '(at-middle-room)
       :del-list '(at-door))
   (op 'grasp-bananas
       :preconds '(at-bananas empty-handed)
       :add-list '(has-bananas)
       :del-list '(empty-handed))
   (op 'drop-ball
       :preconds '(has-ball)
       :add-list '(empty-handed)
       :del-list '(has-ball))
   (op 'eat-bananas
       :preconds '(has-bananas)
       :add-list '(empty-handed not-hungry)
       :del-list '(has-banans hungry))))

;; =========================== maze

(defun make-maze-ops (pair)
  "Make maze ops in both directions."
  (list (make-maze-op (first pair) (second pair))
        (make-maze-op (second pair) (first pair))))

(defun make-maze-op (here there)
  "Make an operator to move between two places."
  (op `(move from ,here to ,there)
      :preconds `((at ,here))
      :add-list `((at ,there))
      :del-list `((at ,here))))

(defparameter *maze-ops*
  (mappend #'make-maze-ops
           '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
             (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
             (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))

(defun find-path (start end)
  "Search a maze for a path from start to end."
  (let ((results (GPS `((at ,start)) `((at ,end)))))
    (unless (null results)
      (cons start (mapcar #'destination
                          (remove '(start) results :test #'equal))))))

(defun destination (action)
  "Find the Y in (executing (move from X to Y))."
  (fifth (second action)))

;; (use *maze-ops*)
;; (gps '((at 1)) '((at 25)))

;; ; ======================= blocks
(defun make-block-ops (blocks)
  (let ((ops nil))
    (dolist (a blocks)
      (dolist (b blocks)
        (unless (equal a b)
          (dolist (c blocks)
            (unless (or (equal c a) (equal c b))
              (push (move-op a b c) ops)))
          (push (move-op a 'table b) ops)
          (push (move-op a b 'table) ops))))
    ops))

(defun move-op (a b c)
  "Make an operator to move A from B to C."
  (op `(move ,a from ,b to ,c)
      :preconds `((space on ,a) (space on ,c) (,a on ,b))
      :add-list (move-ons a b c)
      :del-list (move-ons a c b)))

(defun move-ons (a b c)
  (if (eq b 'table)
      `((,a on ,c))
      `((,a on ,c) (space on ,b))))

;; (gps '((a on table) (b on table) (space on a) (space on b) (space on table)) '((a on b) (b on table)))

;; (gps '((a on b) (b on c) (c on table) (space on a) (space on
;; table)) '((c on b) (b on a))) here, we clobber our goal
;; achieving. but it is still possible. for instance, if we switch the
;; order the ending goals, we can achieve them. So we reorder achieve
;; all and we can get there

;; convert existing data rather than abandon
(mapc #'convert-op *school-ops*)


;; utility functions ====================================
;; taken from peter norvig's info
;; http://norvig.com/paip/auxfns.lisp
(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(defun mappend (fn list)
  (apply #'append (mapcar fn list)))

;; debugging features
(defvar *dbg-ids* nil "Identifiers used by dbg.")
(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))
(defun custom-debug (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))
(defun custom-undebug (&rest ids)
  "Stpo dbg on the ids. With no ids, stop dbg all together."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))
(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ " " *debug-io*))
    (apply #'format *debug-io* format-string args)))


;; EXAMPLE OUTPUT AND USAGE
;; (gps '(son-at-home car-needs-battery have-money have-phone-book) '(son-at-school) *school-ops*)

;; (EXECUTING LOOK-UP-NUMBER)
;; (EXECUTING TELEPHONE-SHOP)
;; (EXECUTING TELL-SHOP-PROBLEM)
;; (EXECUTING GIVE-SHOP-MONEY)
;; (EXECUTING SHOP-INSTALLS-BATTERY)
;; (EXECUTING DRIVE-SON-TO-SCHOOL)
;; SOLVED
;; * (gps '(son-at-home car-needs-battery have-money) '(son-at-school) *school-ops*)

;; NIL
;; * (gps '(son-at-home car-works) '(son-at-school) *school-ops*)

;; (EXECUTING DRIVE-SON-TO-SCHOOL)
;; SOLVED
;; *
