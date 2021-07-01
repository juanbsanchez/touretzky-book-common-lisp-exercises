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



