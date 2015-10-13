; read will only read in one word, as soon as the space key is pressed it executes it
; read-line will wait until the user presses enter then read all text, including spaces, as a string
(defun say-hello ()
  (princ "Please type your name:")
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))

(defun add-five ()
  (princ "Please enter a number:")
  (let ((num (read)))
    (princ "When I add five I get ")
    (princ (+ num 5))))