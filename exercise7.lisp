
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


;; e. To transpose a piece of music up by n half steps, we begin by adding
;; the value n to each note in the piece. Write a function called RAISE
;; that takes a number n and a list of numbers as input and raises each
;; number in the list by the value n. (RAISE 5 ’(5 3 1 3 5 5 5)) should
;; return (10 8 6 8 10 10 10), which is ‘‘Mary Had a Little Lamb’’
;; transposed five half steps from the key of C to the key of F.

(defun raise (n x)
  (mapcar #'(lambda (e) (+ n e)) x))


;; f. Sometimes when we raise the value of a note, we may raise it right
;; into the next octave. For instance, if we raise the triad C-E-G
;; represented by the list (1 5 8) into the key of F by adding five to
;; each note, we get (6 10 13), or F-A-C. Here the C note, represented
;; by the number 13, is an octave above the regular C, represented by
;; 1. Write a function called NORMALIZE that takes a list of numbers
;; as input and ‘‘normalizes’’ them to make them be between 1 and 12.
;; A number greater than 12 should have 12 subtracted from it; a
;; number less than 1 should have 12 added to it. (NORMALIZE ’(6 10 13)) should return (6 10 1).

(defun normalize (x)
  (mapcar #'(lambda (n)
              (if (> n 12) (- n 12)
                  (if (< n 1) (+ n 12)
                      n)))
          x))

(defun normalize (x)
  (mapcar #'(lambda (n)
              (cond (( > n 12) (- n 12))
                    (( < n 12) (+ n 12))
                    (t n)))
          x))

;; g. Write a function TRANSPOSE that takes a number n and a song as
;; input, and returns the song transposed by n half steps.
;; (TRANSPOSE 5 ’(E D C D E E E)) should return (A G F G A A A).
;; Your solution should assume the availability of the NUMBERS,
;; NOTES, RAISE, and NORMALIZE functions. Try transposing
;; ‘‘Mary Had a Little Lamb’’ up by 11 half steps. What happens if
;; you transpose it by 12 half steps? How about −1 half steps?

(defun transpose (n song)
  (mapcar #'first (notes
   (normalize (raise n (mapcar #'first (numbers song)))))))


;; REMOVE-IF AND REMOVE-IF-NOT
;; REMOVE-IF is another applicative operator that takes a predicate as input.
;; REMOVE-IF removes all the items from a list that satisfy the predicate, and returns a list of what's left.

(remove-if #'numbersp '(2 for 1 sale)) ; => (FOR SALE)

;; REMOVE-IF-NOT operator is used more frequently than REMOVE-IF.
;; It works just like REMOVE-IF except it automatically inverts the sense of the predicate.

(remove-if-not #'plusp '(2 0 -4 6 -8 10)) ; => (2 6 10)


;; EXERCISES
;; ---------

;; 7.11. Write a function to pick out those numbers in a list that are greater than
;; one and less than five

(defun pick-out (x)
  (remove-if-not #'(lambda (n) (and (> n 1) (< n 5))) x))

(defun pick-out-alt (x)
  (remove-if-not #'(lambda (x) (< 1 x 5)) x))

;; 7.12. Write a function that counts how many times the word ‘‘the’’ appears
;; in a sentence.

(defun how-many-times (x)
  (length (remove-if-not #'(lambda (x) (equal x 'the)) x)))

(how-many-times '(the red color the green color the yellow color)) ; => 3

;; 7.13. Write a function that picks from a list of lists those of exactly length
;; two.

(defun picks-from-a-list (x)
  (remove-if #'(lambda (e) (not (equal (length e) 2))) x))


;; 7.14. Here is a version of SET-DIFFERENCE written with REMOVE-IF:

(defun my-setdiff (x y)
  (remove-if #'(lambda (e) (member e y))
               x))

;; Show how the INTERSECTION and UNION functions can be written
;; using REMOVE-IF or REMOVE-IF-NOT.

(defun my-intersection (x y)
  (remove-if-not #'(lambda (e) (member e y)) x))

(defun my-union (x y)
  (append x
          (remove-if
           #'(lambda (n)
               (member n x))
           y)))

;; 7.15. In this keyboard exercise we will manipulate playing cards with
;; applicative operators. A card will be represented by a list of form (rank
;; suit), for example, (ACE SPADES) or (2 CLUBS). A hand will be
;; represented by a list of cards.

;;a. Write the functions RANK and SUIT that return the rank and suit of
;; a card, respectively. (RANK ’(2 CLUBS)) should return 2, and
;; (SUIT ’(2 CLUBS)) should return CLUBS.

(defun rank (x)
  (first x))

(defun suit (x)
  (second x))

;; b. Set the global variable MY-HAND:

(setf my-hand
      '((3 hears)
        (5 clubs)
        (2 diamonds)
        (4 diamonds)
        (ace spades)))

;; Now write a function COUNT-SUIT that takes two inputs, a suit and
;; a hand of cards, and returns the number of cards belonging to that
;; suit. (COUNT-SUIT ’DIAMONDS MY-HAND) should return 2.

(defun count-suit (s hand)
  (length (remove-if-not #'(lambda (card) (equal (suit card) s)) hand)))

;; c. Set the variable COLORS

(setf colors
      '((clubs black)
        (diamonds red)
        (hearts red)
        (spades black)))

;; Now write a function COLOR-OF that uses the table COLORS to
;; retrieve the color of a card. (COLOR-OF ’(2 CLUBS)) should
;; return BLACK. (COLOR-OF ’(6 HEARTS)) should return RED.

(defun color-of (x)
  (second (assoc (suit x) colors)))

