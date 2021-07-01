;; CONDITIONALS
;; --------------------------------

;; 4.1 Write a function MAKE-EVEN that makes an odd number even by adding one to it.
;; If the input to MAKE-EVENT is already even, it should be returned unchanged.

(defun make-even (n)
  (if (oddp n)
      (+ n 1)
      n))

(make-even 1) ;; => 2
(make-even 2) ;; => 2


;; 4.2 Write a function FURTHER that makes a positive number larger by adding one to it,
;; and a negative number smaller by subtracting one from it.
;; What does your function do if given the number 0? => add one

(defun further (n)
  (if (< n 0)
      (- n 1)
      (+ n 1)))

(further 2) ;; => 2
(further 0) ;; => 1
(further -3) ;; => -4


;; 4.3 Recall the primitive function NOT:
;; It returns NIL for a true input and T for a false one.
;; Suppose Lisp didn't have a NOT primitive. Show how to write NOT using jus IF and constants. (no other functions)
;; Call your function MY-NOT

(defun my-not (input) (if (equal input nil) t nil))

(my-not nil) ;; => T
(my-not t) ;; => NIL


;; 4.4 Write a function ORDERED that takes two numbers as input and makes a list of them in ascending order.
;; (ORDERED 3 4) should return the list (3 4).
;; (ORDERED 4 3) should also return (3 4)
;; in other words, the first and second inputs should appear in reverse order when the first is greater than the sencond

(defun ordered (x y)
  (if (> x y)
      (list y x)
      (list x y)))

(ordered 3 4) ;; => (3 4)
(ordered 4 3) ;; => (3 4)


;; 4.6 Write a version of the absolute value function MY-ABS using COND instead of IF.

(defun my-abs (x)
  (cond (( < x 0) (-x))
        (t x)))

(my-abs 3) ;; => 3
(my-abs -3) ;; => 3


;; 4.8 Write EMPHASIZE3 which is like EMPHASIZE2 but adds the symbol VERY onto the list it it doesn't know how to emphasize it.
;; For example, EMPHASIZE3 of (LONG DAY) should produce (VERY LONG DAY).
;; What does EMPHASIZE3 of (VERY LONG DAY) produce?

(defun emphasize3 (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
        ((equal (first x) 'bad) (cons 'awful (rest x)))
        (t (cons 'very x))))

(emphasize3 '(good day)) ;; => (great day)
(emphasize3 '(bad day)) ;; => (awful day)
(emphasize3 '(very long day)) ;; => (very very long day)



