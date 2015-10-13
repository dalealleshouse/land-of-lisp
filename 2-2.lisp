; let declars a local variable
; a and b have no meaning outside the function
(let ((a 5)
     (b 10))
  (+ a b))

; flet declares a local function
; the local functions themselves are not aware of each other
(flet ((f (n)
         (+ n 10))
       (g (n)
         (* n 2))
       )
  (print (g (f 5))))

; labels is like flet but it makes the local functional aware of each other
(labels ((a (n)
         (+ n 10))
       (b (n)
         (+ (a  n) 10))
       )
  (b 10))

; this is not from the book, I just wanted to know if I could get it to work
(labels ((pow-iter (n sum iter)
           (if (> iter 1)
             (pow-iter n (* sum n) (1- iter))
             (print sum))))
 (defun power (n pow)
   (pow-iter n n pow)))

(power 5 5)
(power 2 5)
