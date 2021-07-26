
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



;; 11.4. Write an iterative version of LENGTH, called IT-LENGTH.

(defun it-length (x)
  (let ((n 0))
    (dolist (e x n)
      (incf n))))

;; 11.5. Write an iterative version of NTH, called IT-NTH.

(defun it-nth (n x)
  (dotimes (i n (first x))
    (pop x)))

;; 11.6. Write an iterative version of UNION, called IT-UNION. Your function
;; need not return its result in the same order as the built-in UNION
;; function.

(defun it-union (x y)
  (dolist (e x y)
    (unless (member e y)
      (push e y))))

;; 11.8. Write an iterative version of REVERSE, called IT-REVERSE.

(defun it-reverse (x)
  (let ((l (list)))
    (dolist (e x l)
      (push e l))))


;; Example using DO Macro

(defun launch (n)
  (do ((cnt n (- cnt 1)))
      ((zerop cnt) (format t "Blast off!"))
    (format t "~S..." cnt)))

;; 11.9 Show how to write CHECK-ALL-ODD using DO.

(defun check-all-odd (x)
  (do ((z x (rest z)))
      ((null z) t)
  (format t "~&Checking ~S..." (first z))
    (if (evenp (first z)) (return nil))))

;; 11.10. Show how to write LAUNCH using DOTIMES.

(defun launch (n)
  (dotimes (i n 1)
    (format t "~S..." (- n i)))
    (format t "Blast off!"))

(launch 10)
