
;;; LIST DATA STRUCTURES
;;; --------------------

(defun add-to-end (x e)
  "Adds element E to the end of list X."
  (append x (list e)))

(add-to-end '(a b c) 'd) ;; => (A B C D)


;; REVERSE => returns the reversal of a list.
;; Like APPEND, REVERSE is nondestructive. It copies its input rather than modifying it.

;; We can use REVERSE to add an element to the end of a list, as follows.

(defun add-to-end (x y)
  (reverse (cons y (reverse x))))

(add-to-end '(a b c) 'd) ;; => (a b c d)

;; NTHCDR => returns the nth successive cdr of a list.

(nthcdr 1 '(a b c)) ;; => (b c)
(nthcdr 3 '(a b c)) ;; => nil
(nthcdr 4 '(a b c . d)) ;; => Error. D is not a list.

;; NTH => takes the CAR of the NTHCDR of a list.

(defun nth (n x)
  "Returns the Nth element of the list x,
  counting from 0."
  (car (nthcdr n x)))

(nth 0 '(a b c)) ;; => a
(nth 1 '(a b c)) ;; => b
(nth 3 '(a b c)) ;; => nil


;; EXERCISES
;; ---------

;; 6.1 Why is (NTH 4 '(A B C)) equal to NIL? => because last position of the list is 2.
;; 6.2 What is the value of (NTH 3 '(A B C . D)), and why? Error, because D isn't a list.


;; LAST returns the last cons cell of a list.

(last '(all is forgiven)) ;; => (forgiven)
(last nil) ;; => nil
(last '(a b c . d)) ;; (c . d)
(last 'nevermore) ;; => Error! NEVERMORE is not a list.

;; EXERCISES
;; ---------

;; 6.3 What is the value of (LAST '(ROSEBUD))? => (ROSEBUD)
;; 6.4 What is the value of (LAST '((A B C))), and why? => ((A B C)) because is a list inside of a list, so returns that list.


;; REMOVE removes an item from a list. The result returned by REMOVE is a new list, without the deleted items.

(remove 'a '(b a n a n a)) ;; => (b n n)
(remove 1 '( 3 1 4 1 5 9)) ;; => (3 4 5 9)

;; REMOVE is a nondestructive function. It does not change any variables or cons cells when removing elements from a list.
;; REMOVE builds its result out of fresh cons cells by copying (parts of) the list.

(setf spell '( a b r a c a d a b r a)) ;; => (A B R A C A D A B R A)
(remove 'a spell) ;; => ( B R C D B R)
(spell)  ;; => (A B R A C A D A B R A)


;; EXERCISES
;; ---------

;; 6.5 Write an expression to set the global variable LINE to the list (ROSES ARE RED).
;; Then write down what each of the following expressions evaluates to:

(setf line '(ROSES ARE RED))

(reverse line) ;; => (RED ARE ROSES)
(first (last line)) ;; => RED
(nth 1 line) ;; => ARE
(reverse (reverse line)) ;; => ROSES ARE RED
(append line (list (first line))) ;; => (ROSES ARE RED ROSES)
(append (last line) line) ;; => (RED ROSES ARE RED)
(list (first line) (last line)) ;; => (ROSES (RED))
(cons (last line) line) ;; => ((RED) ROSES ARE RED)
(remove 'are line) ;; => (ROSES RED)
(append line '(violets are blue)) ;; => (ROSES ARE RED VIOLETS ARE BLUE)

;; 6.6 Use the LAST function to write a function called LAST-ELEMENT that returns the last element of a list instead of the last cons cell.
;; Write another version of LAST-ELEMENT using REVERSE instead of LAST.
;; Write another version using NTH and LENGTH.

(defun last-element (list)
  (first (last list)))

(defun last-element (list)
  (first (reverse list)))

(defun last-element (list)
  (and list ; to handle NIL correctly
       (nth (- (length list) 1) list)))


;; 6.7 Use REVERSE to write a NEXT-TO-LAST function that returns the
;; next-to-last element of a list. Write another version using NTH.

(defun next-to-last (x)
  (and x
       (nth (- (length x) 2) x)))

(defun last-element (x)
  (second (reverse x)))

;; 6.8 Write a function MY-BUTLAST that returns a list with the last element
;; removed. (MY-BUTLAST ’(ROSES ARE RED)) should return the list
;; (ROSES ARE). (MY-BUTLAST ’(G A G A)) should return (G A G).

(defun my-butlast (x)
  (reverse (cdr (reverse x))))


;; 6.9 What primitive function does the following reduce to?

(defun mystery (x)
  (first (last (reverse x)))) ;; => returns first element of a list


;; 6.10 A palindrome is a sequence that reads the same forwards and
;; backwards. The list (A B C D C B A) is a palindrome; (A B C A B C)
;; is not. Write a function PALINDROMEP that returns T if its input is a palindrome.

(defun palindromep (x)
  (equal x (reverse x)))

;; 6.11 Write a function MAKE-PALINDROME that makes a palindrome out
;; of a list, for example, given (YOU AND ME) as input it should return (YOU AND ME ME AND YOU).

(defun make-palindrome (x)
  (append x (reverse x)))

