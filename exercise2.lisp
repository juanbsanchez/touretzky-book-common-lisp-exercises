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



;; Suppose we wanted to make a function called DUO-CONS that added two elements to the front of a list.
;; Remember that the regular CONS function adds only one element to a list.
;; DUO-CONS would be a function of three inputs.
;; For example, if the inputs were the symbol PATRICK, the symbol SEYMOUR,
;; and the list (MARVIN), DUOCONS would return the list (PATRICK SEYMOUR MARVIN).
;; Show how to write the DUO-CONS function

(defun duo-cons (x y z)
  (cons x (cons y z)))

(duo-const 'patrick 'seymour '(marvin)) ;; => (PATRICK SEYMOUR MARVIN)


;; TWO-DEEPER is a function that surrounds its input with two levels of parentheses.
;;TWO-DEEPER of MOO is ((MOO)). TWO-DEEPER of (BOW WOW) is (((BOW WOW))).
;;Show how to write TWODEEPER using LIST. Write another version using CONS.

(defun two-deeper (x)
  (list (list x)))

(two-deeper 'moo) ;; => ((MOO))
(two-deeper '(BOW WOW)) ;; => (((BOW WOW)))
