(defun mappend (fn the-list)
  "accumulate the results of a mapcar."
  (apply #'append (mapcar fn the-list)))

(defun random-elt (choices)
  "Choose an element at random from list choices."
  (elt choices (random (length choices))))

(defun cross-product (fn xlist ylist)
  "Return a list of all (fn x y) values."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall fn x y)) xlist))
           ylist))
