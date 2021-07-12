
;; RECURSION
;; ---------

;; Here is a recursive function ANYODDP that returns T if any element of a list of numbers id odd.
;; It returns NIL if none of them are:

(defun anyoddp (x)
  (cond ((null x) nil)
        ((oddp (first x)) t)
        (t (anyoddp (rest x)))))

;; 8.2. Show how to write ANYODDP using IF instead of COND.

(defun anyoddp (x)
  (if (null x) nil
      (if (oddp (first x)) t
          (anyoddp (rest x)))))


;; A lisp version of the factorial function

(defun fact (n)
  (cond ((zerop n) 1)
        (t (* n (fact (- n 1))))))


;; If we didn't have LENGTH, we could still count the slices recursively:

(defun count-slices (loaf)
  (cond ((null loaf) 0)
        (t (+ 1 (count-slices (rest loaf))))))


;; 3 rules for solving problems recursively:

;; 1. Know when to stop.
;; 2. Decide how to take one step
;; 3. Beak the journey down into that step plus a smaller journey

;; EXERCISE:

;; 8.4. We are going to write a function called LAUGH that takes a number as
;; input and returns a list of that many HAs. (LAUGH 3) should return
;; the list (HA HA HA). (LAUGH 0) should return a list with no HAs in
;; it, or, as the dragon might put it, ‘‘the empty laugh.’’

(defun laugh (n)
  (cond ((zerop n) nil)
        (t (cons 'ha (laugh (- n 1))))))


;; 8.5. In this exercise we are going to write a function ADD-UP to add up all
;; the numbers in a list. (ADD-UP ’(2 3 7)) should return 12. You
;; already know how to solve this problem applicatively with REDUCE;
;; now you’ll learn to solve it recursively. Before writing ADD-UP we
;; must answer three questions posed by our three rules of recursion.

(defun add-up (x)
  (cond ((null x) 0)
        (t (+ (first x) (add-up (rest x))))))


;; 8.6. Write ALL-ODDP, a recursive function that returns T if all the numbers
;; in a list are odd.

(defun all-oddp (x)
  (if (null x) t
      (if (oddp (first x))
          (all-oddp (rest x)))))

(defun all-oddp-alt (x)
  (cond ((null x) t)
        ((evenp (first x)) nil)
        (t (all-oddp-alt (rest x)))))


;; 8.7. Write a recursive version of MEMBER. Call it REC-MEMBER so you
;; don’t redefine the built-in MEMBER function.

(defun rec-member (x y)
  (cond ((null y) nil)
        ((equal x (first y)) y)
        (t (rec-member x (rest y)))))


;; 8.8. Write a recursive version of ASSOC. Call it REC-ASSOC.

(defun rec-assoc (x y)
  (cond ((null y) nil)
        ((equal x (car (first y)))
         (first y))
        (t (rec-assoc x (rest y)))))


;; 8.9. Write a recursive version of NTH. Call it REC-NTH.

(defun rec-nth (n x)
  (cond ((zerop n) (first x))
        (t (rec-nth (- n 1) (rest x)))))


;; 8.10. For x a nonnegative integer and y a positive integer, x+y equals
;; x+1+(y-1). If y is zero then x+y equals x. Use these equations to build
;; a recursive version of + called REC-PLUS out of ADD1, SUB1,
;; COND and ZEROP. You’ll have to write ADD1 and SUB1 too.

(defun add1 (n) (+ n 1))

(defun sub1 (n) (- n 1))

(defun rec-plus (x y)
  (cond ((zerop y) x)
        (t (rec-plus (add1 x) (sub1 y)))))

