(defun my-average (numbers)
  "demonstration of how we can catch errors.
Displays a message allowing for continuing. We supply a correct value
of 0 in this case but signal that this kinda has no sense."
  (if (null numbers)
      (progn
        (cerror "Use 0 as the average"
                "The empty list has no average")
        0)
      (/ (reduce #'+ numbers) (length numbers))))

(defun sqr (x)
  "multiply X by itself.
But really a demonstration of checking type and allowing for manual
override at the time."
  (check-type x number)
  (* x x))

(defun bank-account (balance)
  "Open a bank account starting with the given balance."
  #'(lambda (action amount)
      (case action
        (deposit (setf balance (+ balance amount)))
        (withdraw (setf balance (- balance amount))))))

(defun show-both (x)
  "example of multiple return values."
  (multiple-value-bind (int rem)
                       (round x)
    (format t "~f = ~d + ~f" x int rem)))

(defun math-quiz (op range n)
  "ask the user a series of math problems."
  (dotimes (i n)
    (problem (random range) op (random range))))

(defun problem (x op y)
  "ask a math problem, read a reply, and say if it is correct"
  (format t "~&How much is ~d ~a ~d?" x op y)
  (if (eql (read) (funcall op x y))
      (princ "Correct!")
      (princ "Sorry, that is not right.")))
