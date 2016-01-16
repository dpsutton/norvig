;; this time we will take a smarter approach
;; our grammar will be placed as such :
;; 
;; Sentence    => Noun-Phrase + Verb-Phrase
;; Noun-Phrase => Article + Noun
;; Verb-Phrase => Verb + Noun-Phrase
;; Article     => the, a, ...
;; Noun        => man, ball, woman, table, ...
;; Verb        => hit, took, saw, liked, ...

(defparameter *simple-grammar*
  '((sentence    -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article     -> the a)
    (Noun        -> man ball woman table)
    (Verb        -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defparameter *bigger-grammar*
  '((sentence    -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP*         -> () (PP PP*))
    (Adj*        -> () (Adj Adj*))
    (PP          -> (Prep noun-phrase))
    (Prep        -> to in by with on)
    (Adj         -> big little blue green adiabatic)
    (Article     -> the a)
    (Name        -> Pat Kim Lee Terry Robin)
    (Noun        -> man ball woman table)
    (Verb        -> hit took saw liked)
    (Pronoun     -> he she it these those that)))

(defvar *grammar* *simple-grammar*)

(setf *grammar* *bigger-grammar*)

(defun generate-tree (phrase)
  "Generate a random sentence or phrase, with a complete parse tree."
  (cond
    ((listp phrase)
     (mapcar #'generate-tree phrase))
    ((rewrites phrase)
     (cons phrase
           (generate-tree (random-elt (rewrites phrase)))))
    (t (list phrase))))

(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond
    ((null phrase) (list nil))
    ((listp phrase)
     (combine-all (generate-all (first phrase))
                  (generate-all (rest phrase))))
    ((rewrites phrase)
     (mappend #'generate-all (rewrites phrase)))
    (t (list (list phrase)))))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
E.g., (combine-all '((a) (b)) '((1) (2)))
-> ((A 1) (B 1) (A 2) (B 2))"
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
           ylist))

(defun generate (phrase)
  "Generate a random sentence or phrase."
  (cond
    ((listp phrase)
     (mappend #'generate phrase))
    ((rewrites phrase)
     (generate (random-elt (rewrites phrase))))
    (t (list phrase))))

;; grammar interacting rules
(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))
(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))
(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

;; utility functions
(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun random-elt (choices)
  "Choose an element at random from list CHOICES."
  (elt choices (random (length choices))))
