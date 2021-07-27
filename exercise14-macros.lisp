
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
