;;;; Wizard's adventrue game from the Land of LISP book

; ' places the interprter in data mode
(defparameter *nodes* '((living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden. this is a well in front of you.))
                        (attic (you are in the attic. there is a giant welding torch in the corner.))))

; We do not referance *nodes* directly to keep the function pure
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

; edges is the proper math term for the lines connecting nodes in a graph
(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

; quasiquoting - kinda like string intrpolation... ` (not ') places it in data mode , places it in code mode
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

; 1) get the list associated with the passed in location - (living-room (garden west door) (attic upstairs ladder))
; 2) cdr gets ride of living-room and leaves a new list ((garden west door) (attic upstairs ladder))
; 3) mapcar applies a higher order function (desribe-path in this instance) to every item in the list ((there is a door going west from here.)(there is a ladder going upstairs from here))
; # is LISP shorthand for denoting a function it is equal to (mapcar (function describe-path) (cdr (assoc location edges)))
; 4) append combines all the list items into a single list (there is a door going west from here. there is a ladder going upstairs from here.)
; we have to use apply because append excpects each list as a seperate parameter and we have one list with multiple list contained in it
; apply passes each item in a list to the target function (append in this instance)
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

; at-loc-p ends in p b/c it's LISP convention to end all T/nil returning functions with p (p for predicate)
; loops through list objs and removes everyone not associated with the specifed loc
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

; (walk 'east)
; 1) find the association between the current location and the edges i.e.: (garden (living-room east door))
; 2) find is a function searches a list for an item and returns that item, the :key is found by the cadr function (or any function you choose)
; in this case, it returns the direction from the edges
; 3) save the result of the find into the next variable. So, next would be equal to (garden (living-room east door))
; 4) if next contains a value the direction is valid so update the current location and call look again, otherwise the direction wasnt' found in
; in the edges so just display a not found message.
(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
          (look))
      '(you cannot go that way.))))

; member checks to see if an item is found in a list of items
; push adds an item to the begining of the list
; we push new things on to *object-locations* without removing the old b\c the assoc function will just return the first item on the list
(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t `(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun game-read ()
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
    '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\? #\! #\.)) (cons item (tweak-text rest t lit)))
            ((eq item #\") (tweak-text rest caps (not lit)))
             (lit (cons item (tweak-text rest nil lit)))
            ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

; prin1-to-string just returns a string instead of printing to the console
; the 1 in prin1 means it will stay on a single line and not add a new line character
; coerce converts the string to a list of characters
(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))