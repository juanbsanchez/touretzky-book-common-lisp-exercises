
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
      '((3 hearts)
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


;; d. Write a function FIRST-RED that returns the first card of a hand
;; that is of a red suit, or NIL if none are.

(defun first-red (hand)
  (first
   (remove-if-not
    #'(lambda (e) (equal (color-of e) 'red))
    hand)))

(defun first-red (hand)
  (find-if #'(lambda (card)
               (equal (color-of card) 'red))
           hand))



;; e. Write a function BLACK-CARDS that returns a list of all the black
;; cards in a hand.

(defun black-cards (hand)
  (remove-if-not
   #'(lambda (card) (equal (color-of card) 'black))
   hand))


;; f. Write a function WHAT-RANKS that takes two inputs, a suit and a
;; hand, and returns the ranks of all cards belonging to that suit.
;; (WHAT-RANKS ’DIAMONDS MY-HAND) should return the list
;; (2 4). (WHAT-RANKS ’SPADES MY-HAND) should return the list (ACE).
;; Hint: First extract all the cards of the specified suit, then use another operator to get the ranks of those cards.

(defun what-ranks (s hand)
  (mapcar #'first
          (remove-if-not
           #'(lambda (card)
               (equal (suit card) s))
           hand)))

;; Set the global variable ALL-RANKS to the list
;; (2 3 4 5 6 7 8 9 10 jack queen king ace)
;; Then write a predicate HIGHER-RANK-P that takes two cards as
;; input and returns true if the first card has a higher rank than the
;; second. Hint: look at the BEFOREP predicate on page 171 of
;; Chapter 6.

(setf all-ranks
      '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun higher-rank-p (card1 card2)
  (beforep (rank card2)
           (rank card1)
           all-ranks))

;; h. Write a function HIGH-CARD that returns the highest ranked card
;; in a hand. Hint: One way to solve this is to use FIND-IF to search a
;; list of ranks (ordered from high to low) to find the highest rank that
;; appears in the hand. Then use ASSOC on the hand to pick the card
;; with that rank. Another solution would be to use REDUCE (defined in the next section) to repeatedly pick the highest card of each pair.

(defun high-card (hand) ; FIND-IF version
  (assoc (find-if
          #'(lambda (r)
              (assoc r hand))
          (reverse all-ranks))
         hand))

(defun high-card (hand)  ; REDUCE version
  (reduce
   #'(lambda (card1 card2)
       (if (higher-rank-p card1 card2)
           card1 card2)) hand))


;; REDUCE => is an applicative operator that reduces the elements of a list into a single result.
;; REDUCE takes a function and a list as input, but unlike the other operators we've seen.
;; REDUCE must be given a function that accepts two inputs.

(reduce #'+ '(1 2 3)) ; => 6

;; We can also apply reduction to lists of lists. To turn a table into a one-level list, we use APPEND as the reducing function.

(reduce #'append '((one un) (two dexu) (three trois))) ; => (ONE UN TWO DEUX THREE TROIS)

;; 7.17. Write a function that, given a list of lists, returns the total length of all
;; the lists. This problem can be solved two different ways.

(defun total-length (x) ; conses a lot
  (length (reduce #'append x)))

(defun total-length-alt (x) ; conses less
  (reduce #'+ (mapcar #'length x)))


;; EVERY => takes a predicate and a list as input. It returns T if there is no element that causes the predicate to return false.

(every #'numberp '(1 2 3 4 5)) ; => T
(every #'numberp '(1 2 A B C 5)) ; => NIL

(every #'(lambda (x) (> x 0)) '(1 2 3 4 5)) ; => T

;; If EVERY is called with NIL as its second argument, it simply returns T,
;; since the empty list has no elements that could fail to satisfy the predicate.

(every #'oddp nil) ; => T
(every #'evenp nil) ; => T

;; EVERY can also operate on multiple lists, given a predicate that accepts
;; multiple inputs.

(every #'> '(10 20 30 40) '(1 5 11 23)) ; => T

;; Since 10 is greater than 1, 20 greater than 5, 30 greater than 11, and 40 greater
;; than 23, EVERY returns T.


;; 7.19. Write a function ALL-ODD that returns T if every element of a list of
;; numbers is odd.

(defun all-odd (x)
  (every #'oddp x))


;; 7.20. Write a function NONE-ODD that returns T if every element of a list of
;; numbers is not odd.

(defun none-odd (x)
  (every #'evenp x))

;; 7.21. Write a function NOT-ALL-ODD that returns T if not every element of
;; a list of numbers is odd.

(defun not-all-odd (x)
  (not (every #'oddp x)))


;; 7.22. Write a function NOT-NONE-ODD that returns T if it is not the case
;; that a list of numbers contains no odd elements.

(defun no-none-odd (x)
  (not (every #'evenp x)))


;; 7.29. If the blocks database is already stored on the computer for you, load
;; the file containing it. If not, you will have to type it in as it appears in
;; Figure 7-3. Save the database in the global variable DATABASE.

(setf database
      '((b1 shape brick)
        (b1 color green)
        (b1 size small)
        (b1 supported-by b2)
        (b1 supported-by b3)
        (b2 shape brick)
        (b2 color red)
        (b2 size small)
        (b2 supports b1)
        (b2 left-of b3)
        (b3 shape brick)
        (b3 color red)
        (b3 size small)
        (b3 supports b1)
        (b3 right-of b2)
        (b4 shape pyramid)
        (b4 color blue)
        (b4 size large)
        (b4 supported-by b5)
        (b5 shape cube)
        (b5 color green)
        (b5 size large)
        (b5 supports b4)
        (b6 shape brick)
        (b6 color purple)
        (b6 size large)))

;; a. Write a function MATCH-ELEMENT that takes two symbols as
;; inputs. If the two are equal, or if the second is a question mark,
;; MATCH-ELEMENT should return T. Otherwise it should return
;; NIL.
;; Thus (MATCH-ELEMENT ’RED ’RED) and (MATCH-ELEMENT ’RED ’?) should return T, but (MATCH-ELEMENT ’RED ’BLUE) should return NIL. Make sure your function works correctly before proceeding further.

(defun match-element (x y)
  (or (equal x y)
      (equal y '?)))


;; b. Write a function MATCH-TRIPLE that takes an assertion and a
;; pattern as input, and returns T if the assertion matches the pattern.
;; Both inputs will be three-element lists. (MATCH-TRIPLE ’(B2 COLOR RED) ’(B2 COLOR ?)) should return
;; T. (MATCH-TRIPLE ’(B2 COLOR RED) ’(B1 COLOR GREEN))
;; should return NIL. Hint: Use MATCH-ELEMENT as a building
;; block.

(defun match-triple (assertion pattern)
  (and (match-element (first assertion) (first pattern))
       (match-element (second assertion) (second pattern))
       (match-element (third assertion) (third pattern))))

(defun match-triple (assertion pattern)
  (every #'match-element
         assertion
         pattern))
