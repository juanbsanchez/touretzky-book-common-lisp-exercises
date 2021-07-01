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


;; Define a predicate called LONGER-THAN that takes two lists as input and returns T if the firs list is longer than the second.

(defun longer-than (list1 list2)
    (> (length list1) (length list2)))

(longer-than '(foo foo foo) '(foo foo)) ;; => T
(longer-than '(foo foo) '(foo foo foo)) ;; => NIL


;; Write a function ADDLENGTH that takes a list as input and returns a new list with the length of the input added onto the front of it.
;; If the input is (MOO GOO GAI PAN), the output should be (4 MOO GOO GAI PAN). What is the result of (ADDLENGTH (ADDLENGTH '(A B C)))?

(defun addlength (my-list)
  (cons (length my-list) my-list))

(addlength '(moo goo gai pan)) ;; => (4 MOO GOO GAI PAN)

(addlength (addlength '(a b c))) ;; => (4 3 A B C)


;; Study this function definition:

(defun call-up (caller callee)
  (list 'hello callee 'this 'is caller 'calling))

;; How many arguments does this function require? 2
;; What are the names of the arguments? caller and callee
;; What is the result of:
(call-up 'FRED 'WANDA) ;; => (hello WANDA this is FRED calling)


;; Here is a variation on the CALL-UP function from the previous problem. What is the result of (CRANK-CALL 'WANDA 'FRED) ?

(defun crank-call (caller callee)
  '(hello callee this is caller calling))

(crank-call 'WANDA 'FRED) ;; => (hello callee this is caller calling)


;; Considerer the following function, paying close attention to the quotes:

(defun scrabble (word)
  (list word 'is 'a 'word))

;; The symbol WORD is used two different ways in this function.
;; What are they? the first word is a variable and second is a symbol

;; What is the result of:
(scrabble 'WORD) ;; => (word is a word)


;; Here's a real confuser:

(defun stooge (larry moe curly)
  (list larry (list 'moe curly) curly 'larry))

;; What does the following evalutate to? It will help to write down what value each variable is bound to and, of course, mind the quotes!

(stooge 'moe 'curly 'larry) ;; => (moe (moe larry) larry larry)


;; Evaluate each of the following lists. If the list causes an error, tell what the error is. Otherwise, write the result of the evaluation.

(cons 'grapes '(of wrath)) ;; => (grapes of wrath)
(list t 'is 'not nil) ;; => (t is not nil)
(first (list moose goose)) ;; => ERROR, Symbol's value as varaible is void: moose
(first (list 'moose 'goose)) ;; => moose
(cons 'home ('sweet 'home)) ;; ERROR, quote sweet


;; Here is a mistery function:

(defun mystery (x)
  (list (second x) (first x)))

;; What result or error is produced by evaluating each of the following?

(mystery '(dancing bear)) ;; => (bear dancing)
(mystery 'dancing 'bear) ;; => ERROR, wrong number of arguments
(mystery '(zowie)) ;; => (nil zowie)
(mystery (list 'first 'second)) ;; => (second first)


;; What is wrong with each of the following function definitions?

(defun speak (x y) (list 'all 'x 'is 'y)) ;; => x and y vars are not evaluated
(defun speak (x) (y) (list 'all x 'is y)) ;; => syntax error on function definition
(defun speak ((x) (y)) (list all 'x is 'y)) ;; => syntax error on function definition


;; Write a predicate FIRSTP that returns T if its first argument (a symbol) is equal to the first element of its second argument (a list).
;; That is, (FIRSTP 'FOO '(FOO BAR BAZ)) should return T.
;; (FIRSTP 'BOING '(FOO BAR BAZ)) should return NIL.

(defun firstp (my-symbol my-list)
  (equal my-symbol (first my-list)))

(firstp 'foo '(foo bar baz)) ;; => T
(firstp 'boing '(foo bar baz)) ;; => NIL



;; Write a function MID-ADD1 that adds 1 to the middle element of a three-element list.
;; For example, (MID-ADD1 '(TAKE 2 COOKIES)) should return the list (TAKE 3 COOKIES).
;; Note: You are not allowed to make MID-ADD1 a function of three inputs. It has to take a single input that is a list of three elements.

(defun mid-add1 (my-list)
  (list (car my-list)
        (+ (cadr my-list) 1)
        (caddr my-list)))

(mid-add1 '(take 2 cookies)) ;; => (take 3 cookies)


;; Write a function F-TO-C that converts a temperature from Fahrenheit to Celsius. The formula for doing the conversion is:
;; Celsius temperature = [5 X (Fahrenheit temperature - 32)] / 9.
;; To go in the opposite direction, the formula is: Fahrenheit temperature = (9/5 x Celsius temperature) + 32.

(defun f-to-c (fah-temperature)
  (/ (* (- fah-temperature 32) 5) 9))

(f-to-c 24.0) ;; => 4.444444444444445
(f-to-c 32.0) ;; => 0.0


;; What is wrong with this function? What does (FOO 5) do?

(defun foo (x) (+ 1 (zerop x))) ;; That functions returns error. Zerop returns t and function + only accept numbers as arguments


;; Write each of the following functions in Church's lambda notation;

;; DOUBLE
(lambda (x) (+ x x))

;; SQUARE
(lambda (x) (* x x))

;; ONEMOREP
(lambda (x y) (equal x (+ y 1)))
