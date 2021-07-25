
;; ASSIGNMENT
;; ----------

;; Suppose we are operating a lemonade stand, and we want to keep track of how
;; many glasses have been sold so far. We keep the number of glasses sold in a
;; global variable, *TOTAL-GLASSES*, which we will initialize to zero this
;; way:

(setf *total-glasses* 0)

;; There is a convention in Common Lisp that global variables should have
;; names that begin and end with an asterisk.*
;; It’s permissible to ignore the
;; asterisk convention when performing some quick calculations with global
;; variables at top level, but when you write a program to manipulate global
;; variables, you should adhere to it. Therefore, we’ll call our global variable
;; *TOTAL-GLASSES*.

;; Now, every time we sell some lemonade, we have to update this variable.
;; We also want to report back how many glasses have been sold so far. Here is
;; a function to do that:

(defun sell (n)
  "Ye Olde Lemonade Stand: Sales by the Glass"
  (setf *total-glasses* (+ *total-glasses* n))
  (format t
	  "~&That makes ~S glasses so far today."
	  *total-glasses*))

;; > (sell 3)
;; That makes 3 glasses so far today
;; NIL

;; > (sell 2)
;; That makes 5 glasses so far today.
;; NIL

;; Instead of incrementing a numeric variable by writing, say, (SETF A (+ A 5)),
;; you can write (INCF A 5). INCF and DECF are special assignment macros
;; for incrementing and decrementing variables. If the increment/decrement
;; value is omitted, it defaults to one.

;; > (setf a 2)
;; 2
;; > (incf a 10)
;; 12
;; > (decf a)
;; 11

;; 10.2. Rewrite the lemonade stand SELL function to use INCF instead of
;; SETF.

(defun sell (n)
  (incf *total-glasses* n)
  (format t
	  "~&That makes ~S glasses so far today."
	  *total-glasses*))

;; 10.3. Modify the MEET function to keep a count of how many people have been met more than once.
;; Store this count in a global variable.

(setf *friends* nil)

(defun meet (person)
  (cond ((equal person (first *friends*))
	 'we-just-met)
	((member person *friends*)
	 'we-know-each-other)
	(t (push person *friends*)
	   'pleased-to-meet-you)))

(setf *met-before* 0)

(defun meet (person)
  (cond ((equal person (first *friends*))
	 (incf *met-before*)
	 'we-just-met)
	((member person *friends*)
	 (incf *met-before*)
	 'we-know-each-other)
	(t (push person *friends*)
	   'pleased-to-meet-you)))

;; 10.4. Write a function FORGET that removes a person from the *FRIENDS* list.
;; If the person wasn’t on the list in the first place, the function should complain.

(defun forget (person)
  (cond ((equal person (first *friends*))
	 (pop *friends*) 'forgotten)
	((member person (cdr *friends*))
	 (list person 'is 'not 'the 'first))
	(t (list 'dont 'know person))))


;; WHEN and UNLESS example:

(defun picky-multiply (x y)
  "Computes X times Y.
  Input X must be odd; Y must be even."
  (unless (oddp x)
    (incf x)
    (format t
	    "~&Changing X to ~S to make it odd." x))
  (when (oddp y)
    (decf y)
    (format t
	    "~&Chaning Y to ~S to make it even" y))
  (* x y))

