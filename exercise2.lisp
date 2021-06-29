;; Write MY-SECOND function using FIRST and REST

(defun my-second (list)
  (first (rest list)))

(my-second '(1 2 3 4 5)) ;; => 2
(my-second '(red blue green)) ;; BLUE



;; Show how to write MY-THIRD using FIRST and two RESTs.

(defun my-thid (list)
  (first (rest (rest list))))

(my-third '(1 2 3 4 5)) ;; => 3
(my-third '(red blue green yellow black white)) ;; => GREEN



;; Show how to write MY-THIRD using SECOND

(defun my-third (list)
  (my-second (rest list)))

(my-third '(1 2 3 4 5)) ;; => 3
(my-third '(reb blue green yellow)) ;; => GREEN


