
;; 13 ARRAYS, HASH TABLES AND PROPERTY LISTS:
;; ------------------------------------------

;; Array:

(setf my-vec '#(tuning violin 440 a))

(type-of my-vec) ; => (SIMPLE-VECTOR 4)

(setf *print-array* t)

my-vec ; => #(TUNNING VIOLIN 440 A)

;; The AREF function is used to access the elements of an array by number
;; just as NTH is used to access the elements of lists.

(aref my-vec 1) ; => VIOLIN

(setf a '#(nil nil nil nil nil))

(setf (aref a 0) 'foo)

(setf (aref a 1) 37)

(setf (aref a 2) 'bar)

a ; => #(FOO 37 BAR NIL NIL)

(length a) ; 5
(reverse a) ; #(NIL NIL BAR 37 FOO)
(find-if #'numberp a) ; 37


;; The Lisp function MAKE-ARRAY creates and returns a new array. The
;; length of the array is specified by the first argument. The initial contents of
;; the array are undefined. Some Common Lisp implementations initialize array
;; elements to zero; others use NIL. To be safe, you should not rely on array
;; elements having any particular initial value unless you have specified one
;; explicitly.
;; MAKE-ARRAY accepts several keyword arguments. The :INITIALELEMENT keyword specifies one initial value to use for all the elements of
;; the array.

(make-array 5 :initial-element 1) ; => #(1 1 1 1 1)

(make-array 5 :initial-contents '(a e i o u)) ; => #(A E I O U)

