
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

 
