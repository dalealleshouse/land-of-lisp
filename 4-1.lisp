(defun my-length (list)
  (if list
      (1+ (my-length (cdr list)))
    0))

(print (my-length (list 1 2 3 4 5)))