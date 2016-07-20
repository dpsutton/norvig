;; remember to (load "auxfns.lisp")

;;; abstract the program
;; (loop (print (eval (read))))

;; abstractoin of a rogram
;; (defun program ()
;;   (loop
;;      (print prompt)
;;      (print (transform (read)))))

;; (defun interactive-interpreter (prompt transformer)
;;   (loop
;;      (print prompt)
;;      (print (funcall transformer (read)))))

;; (defun lisp ()
;;   (interactive-interpreter '> #'eval))

;; (defun eliza ()
;;   (interactive-interpreter 'eliza>
;;                            #'(lambda (x) (flatten (use-eliza-rules x)))))

(defconstant no-bindings '((t . t)))
(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindigs)
        ((segment-pattern-p pattern)
         (segment-matcher pattern input bindings))
        ((single-pattern-p pattern))))
