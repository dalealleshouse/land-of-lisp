;;;; Create a program that uses a binary search

; defparmeter makes a global variable that CAN be changed
; defvar makes a glogbal variable that CANNOT be changed
(defparameter *small* 1)
(defparameter *big* 100)

; ash take a binary number and shifts the bit by the specified number
; 100 -> 01100100
; (ash 100 -1) = 00110010
; 75 = 01001011
;(ash 75 -1) = 00100101
(defun guess-my-number ()
	(ash (+ *small* *big*) -1))

(defun smaller ()
	(setf *big* (1- (guess-my-number)))
	(guess-my-number))

(defun bigger ()
	(setf *small* (1+ (guess-my-number)))
	(guess-my-number))

(defun start-over ()
	(defparameter *small* 1)
	(defparameter *big* 100)
	(guess-my-number))