;; Define a function called add1 that adds one to its input.

(defun add1
    (x)
  (+ x 1))

;; (add1 2) -> 3
;; (add1 3) -> 4


;; Define add2 using add1.

(defun add2
    (x)
  (add1 (add1 x)))

;; (add2 2) -> 4
;; (add2 3) -> 5


;; Define a SUB2 function that subtracts two from its input.

(defun sub2
    (x)
  (- x 2))

;; (sub2 3) -> 1
;; (sub2 4) -> 2


;; Write a predicate TWOMOREP that returns T if its first input is exactly two more than its second input. Use the ADD2 function in your definition of TWOMOREP.

(defun twomorep
    (x y)
  (equal (add2 x) y))

;; (twomorep 3 5) -> T
;; (twomorep 5 5) -> NIL


;; Find a way to write the TWOMOREP predicate using SUB2 instead of ADD2.

(defun twomorep
    (x y)
  (equal (sub2 y) x))

;; (twomorep 5 7) -> T
;; (twomorep 7 7) -> NIL


;; The average of two numbers is half their sum. Write the AVERAGE function.

(defun average
    (x y)
  (/ (+ x y) 2))

;; (average 4 10) -> 7
;; (average 6 10) -> 8


;; Write a MORE-THAN-HALF-P predicate that returns T if its first input is more than half of its second input.

(defun more-than-half-p
    (x y)
  (> x (/ y 2)))

;; (more-than-half-p 2 4) -> NIL
;; (more-than-half-p 8 14) -> T

