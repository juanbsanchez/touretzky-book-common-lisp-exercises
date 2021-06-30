;; Write MY-SECOND function using FIRST and REST

(defun my-second (list)
  (first (rest list)))

(my-second '(1 2 3 4 5)) ;; => 2
(my-second '(red blue green)) ;; BLUE



;; Show how to write MY-THIRD using FIRST and two RESTs.

(defun my-third (list)
  (first (rest (rest list))))

(my-third '(1 2 3 4 5)) ;; => 3
(my-third '(red blue green yellow black white)) ;; => GREEN



;; Show how to write MY-THIRD using SECOND

(defun my-third (list)
  (my-second (rest list)))

(my-third '(1 2 3 4 5)) ;; => 3
(my-third '(reb blue green yellow)) ;; => GREEN



;; Write a function that takes any two inputs and makes a list of them using cons

(defun my-cons (x y)
  (cons x (cons y nil)))

(my-cons 'foo 'fee) ;; => (FOO FEE)
(my-cons '(foo) 'fee) ;; => ((FOO) FEE)



;; Write a function that takes four inputs and returns a two-elements nested list.
;; The first element should be a list of the first two inputs,
;; and the second element a list of the last two inputs.

(defun two-lists (a b c d)
  (cons (list a b)
        (cons (list c d) nil)))

(two-lists 'a 'b 'c 'd) ;; => ((A B) (C D))
(two-lists 1 2 3 4) ;; => ((1 2) (3 4))

