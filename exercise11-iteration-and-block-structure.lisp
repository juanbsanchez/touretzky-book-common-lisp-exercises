
;; ITERATION AND BLOCK STRUCTURE
;; -----------------------------

;; DOTIMES example:

(dotimes (i 4)
  (format t "~&I is ~S." i))

;; DOLIST example

(dolist (x '(red blue green) 'flowers)
  (format t "~&Roses are ~S." x))

;; DOLIST example using RETURN

(defun find-first-odd (list-of-numbers)
  (dolist (e list-of-numbers)
    (format t "~&Testing ~S..." e)
  (when (oddp e)
    (format t "found and odd number.")
    (return e))))

;; DOLIST example

(defun check-all-odd (list-of-numbers)
  (dolist (e list-of-numbers t)
    (format t "~&Checking ~S..." e)
    (if (not (oddp e)) (return nil))))


;; 11.1. Write an iterative version of the MEMBER function, called ITMEMBER. It should return T if its first input appears in its second
;; input; it need not return a sublist of its second input.

(defun itmember (x y)
  (dolist (e y)
    (if (equal x e) (return t))))

;; 11.2. Write an iterative version of ASSOC, called IT-ASSOC.

(defun it-assoc (x y)
  (dolist (e y)
    (when (equal x (first e)) (return e))))

;; 11.3. Write a recursive version of CHECK-ALL-ODD. It should produce the
;; same messages and the same result as the preceding iterative version.

(defun check-all-odd (list-of-numbers)
  (cond ((null list-of-numbers) t)
	(t (format t "~&Checking ~S..."
		   (first list-of-numbers))
	   (unless (evenp (first list-of-numbers))
	     (check-all-odd (rest list-of-numbers))))))



