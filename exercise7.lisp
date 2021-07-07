
;; 7. APPLICATIVE PROGRAMMING
;; --------------------------

;; MAPCAR => It applies a function to each element of a list, one at a time, and returns a list of the results.

(defun square (n) (* n n))

(square '(1 2 3 4 5)) ;; => ERROR, Wrong type input.

(mapcar #'square '(1 2 3 4 5)) ;; => (1 4 9 16 25)

;; EXERCISES
;; ---------

;; 7.1 Write an ADD1 function that adds one to its input. Then write an
;; expression to add one to each element of the list (1 3 5 7 9).

(defun add1 (x)
  (+ 1 x))

(mapcar #'add1 '(1 3 5 7 9)) ; => (2 4 6 8 10)

;; 7.2 Let the global variable DAILY-PLANET contain the following table:

(setf daily-planet
      '((olsen jimmy 123-76-4535 cub-reporter)
        (kent clark 089-52-6787 reporter)
        (lane lois 951-26-1438 reporter)
        (white perry 355-16-7439 editor)))

;; Each table entry consists of a last name, a first name, a social security
;; number, and a job title. Use MAPCAR on this table to extract a list of
;; social security numbers.

(mapcar #'third daily-planet)

;; 7.3 Write an expression to apply the ZEROP predicate to each element of
;; the list (2 0 3 4 0 -5 -6). The answer you get should be a list of Ts and
;; NILs.

(mapcar #'zerop '(2 0 3 4 0 -5 -6)) ; => (NIL T NIL NIL T NIL NIL)

;; 7.4 Suppose we want to solve a problem similar to the preceding one, but
;; instead of testing whether an element is zero, we want to test whether it
;; is greater than five. We can’t use > directly for this because > is a
;; function of two inputs; MAPCAR will only give it one input. Show
;; how first writing a one-input function called GREATER-THAN-FIVE-P would help.

(defun greater-than-five-p (x)
  (> x 5))

(mapcar #'greater-than-five-p '(2 0 3 4 0 -5 -6))


;; LAMBDA => Lambda expressions look similar to DEFUNs, except that the function name is missing and the word LAMBDA appears in place of DEFUN.

;; EXERCISES
;; ---------

;; 7.5. Write a lambda expression to subtract seven from a number.

(lambda (n) (- n 7))

;; 7.6 Write a lambda expression that returns T if its input is T or NIL, but
;; NIL for any other input

(lambda (x)
  (or (null x) (equal x t)))

;; 7.7. Write a function that takes a list such as (UP DOWN UP UP) and
;; "flips" each element, returning (DOWN UP DOWN DOWN).
;; Your function should include a lambda expression that knows how to flip an individual element,
;; plus an applicative operator to do this to every element of the list.

(defun flips (x)
  (mapcar #'(lambda (x)
              (if (equal x 'up) 'down 'up)) x))

(defun flip-element (e)
  (if (equal e 'up) 'down 'up))

(defun flip (x)
  (mapcar #'flip-element x))

