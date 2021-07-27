
;; 14 MACROS
;; ---------

;; Defining a MACRO
;; ----------------

;; Macros are defined with DEFMACRO. Its syntax is similar to DEFUN. Let’s
;; define a simplified version of INCF to increment ordinary variables. Our
;; macro will take a variable name as input and construct an expression to
;; increment that variable by one.

(defmacro simple-incf (var)
  (list 'setq var (list '+ var 1)))

(setf a 4)
(simple-incf a)

;; Now let’s modify SIMPLE-INCF to accept an optional second argument
;; specifying the amount by which to increment the variable. We do this with the
;; &OPTIONAL lambda-list keyword. (Optional arguments were explained in
;; Advanced Topics section 11.13.) The default amount to increment the
;; variable will be one.

(defmacro simple-incf (var &optional (amount 1))
  (list 'setq var (list '+ var amount)))

(simple-incf a 3)

;; EXERCISE
;; --------

;; 14.3. Write a SET-NIL macro that sets a variable to NIL.

(defmacro set-nil (var)
  (list 'setf var nil))

(setf b 3)
b ; => 3
(set-nil b)
b ; => nil

(defun set-nil-f (var)
  (setf var nil))

(setf b 4)
b ; => 4
(set-nil-f b)
b ; => 4

;; There are three important
;; differences between ordinary functions and macro functions:
;; 1. The arguments to ordinary functions are always evaluated; the arguments to
;; macro functions are not evaluated.
;; 2. The result of an ordinary function can be anything at all; the result returned
;; by a macro function must be a valid Lisp expression.
;; 3. After a macro function returns an expression, that expression is
;; immediately evaluated. The results returned by ordinary functions do not
;; get evaluated.

;; The backquote character (‘) is analogous to quote, in that both are used to
;; quote lists. However, inside a backquoted list, any expression that is preceded
;; by a comma is considered to be ‘‘unquoted,’’ meaning the value of the
;; expression rather than the expression itself is used

(setf name 'John)

`(i gave ,name about ,(* 25 8) dollars)

;; We can use backquote to write a more concise version of the SIMPLEINCF macro:

(defmacro simple-incf (var &optional (amount 1))
  `(setq ,var (+ ,var ,amount)))

(setf c 3)
(simple-incf c 5) ; => 8

;; 14.4. Write a macro called SIMPLE-ROTATEF that switches the value of
;; two variables. For example, if A is two and B is seven, then (SIMPLEROTATEF A B) should make A seven and B two. Obviously, setting
;; A to B first, and then setting B to A won’t work. Your macro should
;; expand into a LET expression that holds on to the original values of the
;; two variables and then assigns them their new values in its body.

(defmacro simple-rotatef (var1 var2)
  `(let ((tmp1 ,var1)
	 (tmp2 ,var2))
     (setf ,var1 tmp2)
     (setf ,var2 tmp1)))

(setf a 2)
(setf b 3)

(simple-rotatef a b)

;; In the example below, TWO-FROM-ONE is a macro that takes a function
;; name and another object as arguments; it expands into a call to the function
;; with two arguments, both of which are the quoted object.

(defmacro two-from-one (func object)
  `(,func ',object ',object))

(two-from-one cons aardvark) ; => (AARDVARK . AARDVARK)


;; 14.5. Write a macro SET-MUTUAL that takes two variable names as input
;; and expands into an expression that sets each variable to the name of
;; the other. (SET-MUTUAL A B) should set A to ’B, and B to ’A.

(defmacro set-mutual (var1 var2)
  `(progn
     (setf ,var1 ',var2)
     (setf ,var2 ',var1)))

(set-mutual a b)

;; Another feature of backquote is that if a template element is preceded by a
;; comma and an at sign (,@), the value of that element is spliced into the result
;; that backquote constructs rather than being inserted. (The value of the element
;; must be a list.) If only a comma is used, the element would be inserted as a
;; single object, resulting in an extra level of parentheses.

(setf name 'fred)

(setf address '(16 maple drive))

`(,name lives at ,address now) ; Inserting
`(,name lives at ,@address now) ; Splicing

;; Here is the definition of SET-ZERO. It uses MAPCAR to construct a
;; SETF expression for each variable in the argument list. The SETF expressions
;; are then spliced into the body of the PROGN. Also, the final expression in the
;; PROGN’s body is a quoted list constructed by splicing. If there were a plain
;; comma there instead of a comma and at sign combination, the result would be
;; (ZEROED (A B C)).

(defmacro set-zero (&rest variables)
  `(progn ,@ (mapcar #'(lambda (var)
			 (list 'setf var 0))
		     variables)
	     '(zeroed ,@variables)))

(set-zero a b c)

;; 14.6. Write a macro called VARIABLE-CHAIN that accepts any number of
;; inputs. The expression (VARIABLE-CHAIN A B C D) should expand
;; into an expression that sets A to ’B, B to ’C, and C to ’D.


(defmacro variable-chain (&rest vars)
  `(progn
     ,@(do ((v vars (rest v))
	    (res nil))
	   ((null (rest v)) (reverse res))
	 (push `(setf ,(first v)
		      ',(second v))
	       res))))

;; Let’s take a look at the effect of COMPILE on the running time of a simple
;; function. This function returns the smallest integer larger than the square root
;; of its input. It computes the result in a very tedious way, but that will help us
;; measure the speedup achieved by compilation.

(defun tedious-sqrt (n)
  (dotimes (i n)
    (if (> (* i i) n) (return i))))

(time (tedious-sqrt 5000000))

(compile 'tedious-sqrt)

(time (tedious-sqrt 5000000))

;; Example of destructuring

;; macro takes two pairs as input and returns an expression that produces four pairs:

(defmacro mix-and-match (p q)
  (let ((x 1 (first p))
	(y1 (second p))
	(x2 (first q))
	(y2 (second q)))
    `(list '(,x1 ,y1)
	   '(,x1 ,y2)
	   '(,x2 ,y1)
	   '(,x2 ,y2))))

(defmacro mix-and-match ((x1 y1) (x2 y2))
  `(list '(,x1 ,y1)
	 '(,x1 ,y2)
	 '(,x2 ,y1)
	 '(,x2 ,y2)))

;; Destructuring is only available for macros, not ordinary functions. It is
;; particularly useful for macros that define new bits of control structure with a
;; complex syntax. The DOVECTOR macro that follows is modeled after
;; DOTIMES and DLIST. It steps an index variable through successive elements
;; of a vector. The macro uses destructuring to pick apart the index variable
;; name, the vector expression, and the result form.

(defmacro dovector ((var vector-exp
		     &optional result-form)
		    &body body)
  `(do* ((vec-dov ,vector-exp)
	 (len-dov (length vec-dov))
	 (i-dov 0 (+ i-dov 1))
	 (,var nil))
	((equal i-dov len-dov) ,result-form)
     (setf ,var (aref vec-dov i-dov))
    ,@body))

;; > (dovector (x '#(foo bar baz))
;;     (format t "~&X is ~S" x))
;; X is FOO
;; X is BAR
;; X is BAZ
;; NIL
