;; EXERCISE 5. VARIABLES AND SIDE EFFECTS.
;; ---------------------------------------

;; 5.1. Rewrite function POOR-STYLE to create a new local variable Q using
;; LET, instead of using SETF to change P. Call your new function
;; GOOD-STYLE.

(defun good-style (p)
  (let ((q (+ p 5)))
    (list 'result 'is q)))

(good-style 8) ;; => (result is 13)


