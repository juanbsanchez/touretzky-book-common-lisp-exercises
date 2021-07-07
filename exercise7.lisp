
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

(defun flips (e)
  (mapcar #'(lambda (x)
              (if (equal x 'up) 'down 'up)) e))

(defun flip-element (e)
  (if (equal e 'up) 'down 'up))

(defun flip (x)
  (mapcar #'flip-element x))


;; FIND-IF => is another applicative operator. If you give FIND-IF a predicate and a list as input,
;; it will find the first elemenet of the list for which the predicate returns true (any non-NIL value).

(find-if #'oddp '(2 4 6 7 8 9)) ; => 7

(find-if #'(lambda (x) (> x 3)) '(3 4 6 7 8 9)) ; => 4

;; ASSOC searches for a table entry with a specified key. We can write a simple
;; version of ASSOC that uses FIND-IF to search the table.

(defun my-assoc (key table)
  (find-if #'(lambda (entry)
               (equal key (first entry)))
             table))

;; 7.8. Write a function that takes two inputs, X and K, and returns the first
;; number in the list X that is roughly equal to K. Let’s say that ‘‘roughly
;; equal’’ means no less than K−10 and no more than K+10.

(defun roughly-equal (e k)
  (and (not (< e (- k 10)))
       (not (> e (+ k 10)))))

(defun find-first-roughly-equal (x k)
  (find-if #'(lambda (e) (roughly-equal e k)) x))

(find-first-roughly-equal '(25 32 5 3) 12) ; => 5
(find-first-roughly-equal '(25 32 5 3) -7) ; => 3

;; 7.9. Write a function FIND-NESTED that returns the first element of a list
;; that is itself a non-NIL list.

(defun find-nested (x)
  (find-if #'consp x))

(find-nested '(2 3 (4) (5))) ; => (4)

;; 7.10. In this exercise we will write a program to transpose a song from one
;; key to another. In order to manipulate notes more efficiently, we will
;; translate them into numbers. Here is the correspondence between notes
;; and numbers for a one-octave scale:

;; a. Write a table to represent this information. Store it in a global
;; variable called NOTE-TABLE.

(setf note-table
      '((c 1)
        (c-sharp 2)
        (d 3)
        (d-sharp 4)
        (e 5)
        (f 6)
        (f-sharp 7)
        (g 8)
        (g-sharp 9)
        (a 10)
        (a-sharp 11)
        (b 12)))

;; b. Write a function called NUMBERS that takes a list of notes as input and returns the corresponding list of numbers.
;; (NUMBERS ’(E D C D E E E)) should return (5 3 1 3 5 5 5).
;; This list represents the first seven notes of ‘‘Mary Had a Little Lamb.’’

(defun numbers (x)
  (mapcar #'(lambda (e)
              (cdr (assoc e note-table)))
          x))

;; c. Write a function called NOTES that takes a list of numbers as input and returns the corresponding list of notes.
;; (NOTES ’(5 3 1 3 5 5 5)) should return (E D C D E E E).
;; Hint: Since NOTE-TABLE is keyed by note, ASSOC can’t look up numbers in it; neither can RASSOC, since the elements are lists, not dotted pairs.
;; Write your own table-searching function to search NOTE-TABLE by number instead of by note

(defun flip-list (x)
  (mapcar #'reverse x))

(defun notes (x)
  (mapcar #'(lambda (e)
              (cdr (assoc e (flip-list note-table))))
          x))

