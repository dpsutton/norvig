;;; eliza1.lisp
;; (load "auxfns.lisp")
;; To use the code, edit any of the files or add new files.  You will
;; always have to do <tt>(load "auxfns.lisp")</tt> first, and you will
;; typically have to do <tt>(requires "<i>file</i>")</tt>, for various
;; instances of <i>file</i> that you want to use.

;; <p>
;; The function "requires"
;; is used for a primitive form of control over what files require other
;; files to be loaded first.  If "requires" does not work properly on
;; your system you may have to alter its definition, in the file
;; "auxfns.lisp".  For more complicated use of these files, you should
;; follow the guidelines for organizing files explained in Chapter 24.

;; <p>
;; The function <tt>do-examples</tt>, which takes as an argument either <tt>:all</tt>
;; or a chapter number or a list of chapter numbers, can be used to see examples
;; of the use of various functions.  For example, <tt>(do-examples 1)</tt> shows
;; the examples from chapter 1.

;; pattern (i need a X)
;; response (what would it mean to you if you got a x?)

;; input (i need a vacation)
;; transformation: (what would it mean to you if you got a vacation?)


                                        ; (defconstant fail nil "Indicates pat-match failure")
;; (defun pat-match (pattern input &optional (bindings no-bindings))
;;   "Match pattern against input in the context of the bindings"
;;   (cond ((eq bindings fail) fail)
;;         ((variable-p pattern) (match-variable pattern input bindings))
;;         ((eql pattern input) bindings)
;;         ((and (consp pattern) (consp input))
;;          (pat-match (rest pattern) (rest input)
;;                     (pat-match (first pattern) (first input) bindings)))
;;         (t fail)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) par to a binding list."
  (cons (cons var val)
        ;; once we add a "real" binding,
        ;; we can get rid of th dummy no-bindings
        (if (eq bindings no-bindings)
            nil
            bindings)))

(defun segment-pattern-p (pattern)
  "Is this a segment mathing pattern: ((?* var) .pat)"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Math pattern against input in the context of the bindings."
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)
         (segment-match pattern input bindings))
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input)
                               bindings)))
        (t fail)))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  ;;  CL-USER> (pat-match '((?* ?x) is a (?* ?Y)) '(what he is is a fool))
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        ;; we assume that pat starts with a constant
        ;; in other words, a pattern can't have 2 consecutive vars
        (let ((pos (position (first pat) input
                             :start start :test #'equal)))
          (if (null pos)
              fail
              (let ((b2 (pat-match pat (subseq input pos) bindings)))
                ;; if this match failed, try another one
                ;; if it worked, check that the variables match
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    (match-variable var (subseq input 0 pos) b2))))))))

(defun segment-match (pattern input bindings &optional (start 0))
  "match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        ;; otherwise we assume that pat starts with a constant
        ;; as no good way for two consecutive variables
        (let ((pos (position (first pat) input
                             :start start :test #'equal)))
          (if (null pos)
              fail
              (let ((b2 (pat-match
                         pat (subseq input pos)
                         (match-variable var (subseq input 0 pos)
                                         bindings))))
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

;; example rule: first is pattern, rest is collection of responses of
;; which we will choos one randomly in the future
;; (((?* ?X) I want (?* ?Y))
;; (What would it mean if you got ?Y) (Why do you want ?Y)
;; (Suppose you got ?y soon))

(defparameter *eliza-rules*
  '((((?* ?x) hello (?* ?y))
     (How do you do. Please state your problem.))
    (((?* ?x) I want (?* ?y))
     (What would i tmean if you got ?y)
     (Why do you want ?y) (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (Do you really think its likely that ?y) (Do you wish that ?y)
     (What do you think about ?y) (Really-- if ?y))
    (((?* ?x) no (?* ?y))
     (Why not?) (You are being a bit negative)
     (Are you saying "NO" just to be negative))
    (((?* ?x) I was (?* ?y))
     (Were you really?) (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
    (((?* ?y) I feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))))

(defun eliza ()
  "Respond to user input using pattern matching rules."
  (loop
     (print 'eliza>)
     (write (flatten (use-eliza-rules (read))) :pretty t)))

(defun use-eliza-rules (input)
  "Find some rule with hich to transform the input."
  (some #'(lambda (rule)
            (let ((result (pat-match (rule-pattern rule) input)))
              (if (not (eq result fail))
                  (sublis (switch-viewpoint result)
                          (random-elt (rule-responses rule))))))
        *eliza-rules*))

(defun switch-viewpoint (words)
  "Change I to you and vice versa, and so on"
  (sublis '((I . you) (you . I) (me . you) (am . are))
          words))
