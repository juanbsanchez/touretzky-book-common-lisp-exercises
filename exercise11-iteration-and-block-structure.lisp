
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
  (dotimes (i n)
    (format t "~S..." (- n i)))
    (format t "Blast off!"))


;; Infinite loop with DO, specifying NIL as the termination test.
;; If user types something other than a number, the function prints an error message
;; and again watis for input. If user does type a number, the function exists the loop using RETURN to return the number.

(defun read-a-number ()
  (do ((answer nil))
      (nil)
    (format t "~&Please type a number: ")
    (setf answer (read))
    (if (numberp answer)
	(return answer))
    (format t
	    "~&Sorry, ~S is not a number. Try again."
	    answer)))


;; In this keyboard exercise we will explore some properties of single- and
;; double-stranded DNA, or deoxyribonucleic acid. DNA, and the related
;; molecule RNA, make up the genetic material found in viruses and every type of cell, from bacteria to people. A strand of DNA is very much like a chain of
;; cons cells. The elements of the chain are of four types, corresponding to the
;; four bases adenine, thymine, guanine, and cytosine. We will represent a
;; strand of DNA by a list of bases. The list (A G G T C A T T G) corresponds
;; to a strand that is nine bases long. The first base being adenine and the next two
;; guanine. Here is a schematic diagram of the strand:


;; ------------------
;; ! ! ! ! ! ! ! ! !
;; A G G T C A T T G

;; Each of the four bases has a complement with which it can form a pair.
;; Adenine pairs with thymine, while guanine pairs with cytosine. Two single
;; strands of DNA can combine to form double-stranded DNA (whose shape is
;; the famous ‘‘double helix’’) when each of their corresponding bases are
;; complementary. The strand (A G G T C A T T G) and the strand (T C C A G
;; T A A C) are complementary, for example. Double-stranded DNA looks like
;; this:

;; -----------------
;; ! ! ! ! ! ! ! ! !
;; A G G T C A T T G
;; . . . . . . . . .
;; . . . . . . . . .
;; T C C A G T A A C
;; ! ! ! ! ! ! ! ! !
;; -----------------

;; 11.22. Write iterative solutions to all parts of this exercise that require
;; repetitive actions.
;; a. Write a function COMPLEMENT-BASE that takes a base as input
;; and returns the matching complementary base. (COMPLEMENTBASE ’A) should return T; (COMPLEMENT-BASE ’T) should
;; return A; and so on.

;; cond solution
(defun complement-base (x)
  (cond ((equal x 'a) 't)
	((equal x 'g) 'c)
	((equal x 't) 'a)
	((equal x 'c) 'g)
	(t nil)))

;; assoc solution
(defun complement-base (base)
  (second
   (assoc base '((a t) (t a) (g c) (c g)))))

;; iterative solution
(setf base
      '((a t)
	(t a)
	(g c)
	(c g)))

(defun complement-base (x)
  (dolist (e base)
    (if (equal x (first e)) (return (second e)))))


;; b. Write a function COMPLEMENT-STRAND that returns the
;; complementary strand of a sequence of single-stranded DNA.
;; (COMPLEMENT-STRAND ’(A G G T)) should return (T C C A).

(defun complement-strand (strand)
  (do ((s strand (rest s))
       (result nil
	       (cons (complement-base (first s))
		     result)))
      ((null s) (reverse result))))

(complement-strand '(a g g t))

