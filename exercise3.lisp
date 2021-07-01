;; What does (NOT (EQUAL 3 (ABS -3))) ?

(EQUAL 3 (ABS -3)) ;; => T
(NOT T) ;; => NIL
;; returns NIL


;; Write an expression in EVAL notation to add 8 to 12 and divide the result by 2.

(/ (+ 8 12) 2) ;; => 10


;; You can square a number by multiplying it by itself.
;; Write an expression in EVAL notation to add the square of 3
;; and the square of 4.

(+ ( * 3 3) ( * 4 4)) ;; => 25


;; Write definitions for HALF, CUBE, and ONEMOREP using DEFUN.
;; (THE CUBE function should take a number n as input and return n^3).

(defun half (n)
  (/ n 2))

(half 3.0) ;; => 1.5
(half 4) ;; => 2


(defun cube (n)
  (* n n n))

(cube 3) ;; => 27
(cube 4) ;; => 64


(defun onemorep (x y)
  (equal x (+ y 1)))

(onemorep 3 4) ;; => NIL
(onemorep 4 3) ;; => T


;; Define a function PYTHAG that takes two inputs, x and y, and returns the square root of x^2+y^2.
;; You may recognize this as Pythagora's formula for computing the length of the hypotenuse of a right triangle
;; given the lengths of the other two sides. (PYTHAG 3 4) should return 5.0

(defun pythag (x y)
  (sqrt (+ (* x x) (* y y))))

(pythag 3 4) ;; => 5.0


;; Define a function MILES-PER-GALLON that takes three inputs, called
;; INITIAL-ODOMETER-READING, FINAL-ODOMETER-READING,
;; and GALLONS-CONSUMED, and computes the number of miles traveled per gallon of gas

(defun miles-per-gallon
    (initial-odometer-reading
     final-odometer-reading
     gallons-consumed)
  (/ (- final-odometer-reading
          initial-odometer-reading)
      gallons-consumed))

(miles-per-gallon 10 100 10) ;; => 9
(miles-per-gallon 55 60 3.0) ;; => 1.6666666666666667


;; The following expressiones evalutate without any errors. Write the results.

(cons 5 (list 6 7)) ;; => (5 6 7)

(cons 5 '(list 6 7)) ;; => (5 list 6 7)

(list 3 'from 9 'gives (- 9 3)) ;; => (3 from 9 gives 6)

(+ (length '(1 foo 2 moo))
   (third '(1 foo 2 moo))) ;; => 6

(rest '(cons is short for construct)) ;; => (is short for construct)


;; The following expressiones all result in errors.
;; Write down the type of error that occurs, explain how the error arose (for example,
;; missing quote, quote in wrong place), and correct the expression by changing only the quotes.

(third (the quick brown fox))

;; ERROR => Wrong number of arguments.
;; CAUSE => missing quote
;; CORRECT:
(third '(the quick brown fox)) ;; => brown

;; ---------------------------------

(list 2 and 2 is 4)

;; ERROR => Symbol's value as variable is void: and
;; CAUSE => missing quote
;; CORRECT:
(list '2 'and '2 'is '4) ;; => (2 and 2 is 4)

;; ---------------------------------

(+ 1 '(length (list t t t t)))

;; ERROR => Wrong type argument
;; CAUSE => quote in wrong place
;; CORRECT:

(+ 1 (length (list t t t t))) ;; => 5

;; ---------------------------------

(cons 'patrick (seymour marvin))

;; ERROR => Symbol's function definition is void: seymour
;; CAUSE => missing quote
;; CORRECT:

(cons 'patrick '(seymour marvin)) ;; => (patrick seymour marvin)

;; ---------------------------------

(cons 'patrick (list seymour marvin))

;; ERROR => Symbol's value as variable is void: seymour
;; CAUSE => missing quote
;; CORRECT:

(cons 'patrick (list 'seymour 'marvin)) ;; => (patrick seymour marvin)
