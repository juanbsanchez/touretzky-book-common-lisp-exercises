;; INPUT / OUTPUT

;; Format T function normally returns NIL, but as a side effect it causes things to be written to display or to a file.

;; Thee first argument to FORMAT should be the symbol T when we want to write to the display.

(format t "Hi, mom!")
;; Hi, mom!
;; NIL

(format t "Fruit flies ~%~%like banananas.")
;; Fruit flies

;; like bananas.
;; NIL


;; The ~& directive tells FORMAT to move to a new line unless it knows it is already at the beginning of a new line.

(format t "~&Mary had a little bat.")
;; Mary had a little bat.
;; NIL

;; Directive ~S inserts the printed representation of a Lisp object.

(format t "From ~S to ~S in ~S minutes!"
	'boston '(new york) 55)

;; From Boston to (NEW YORK) in 55 minutes!
;; NIL

(defun square-talk (n)
  (format t "~&~S squared is ~S" n (* n n)))

(square-talk 10)
;; 10 squared is 100
;; NIL

(mapcar #'square-talk '(1 2 3 4 5))
;; 1 squared is 1
;; 2 squared is 4
;; 3 squared is 9
;; 4 squared is 16
;; 5 squared is 25
;; (NIL NIL NIL NIL NIL)

;; The ~A directive prints an object without using escape characters.

(defun test (x)
  (format t "~&With escape characters: ~S" x)
  (format t "~&Without escape characters: ~A" x))

;; > (test "Hi, mom")
;; With escape characters: "Hi, mom"
;; Without escape characters: Hi, mom
;; NIL

;; 9.1. Write a function to print the following saying on the display: ‘‘There
;; are old pilots, and there are bold pilots, but there are no old bold
;; pilots.’’ Your function should break up the quotation into several lines.

(defun print-sentece ()
  (format t "~&There are old pilots,
~&and there are bold pilots, 
~&but there are no old bold pilots"))

;; 9.2. Write a recursive function DRAW-LINE that draws a line of a specified
;; length by doing (FORMAT T "*") the correct number of times.
;; (DRAW-LINE 10) should produce *********

(defun draw-line (n)
  (cond ((zerop n) (format t "~%"))
	(t (format t "*") (draw-line (- n 1)))))
