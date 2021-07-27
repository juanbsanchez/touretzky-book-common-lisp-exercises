
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

;; Strings are actually a special type of vector. Thus, such functions as
;; LENGTH, REVERSE, and AREF, which work on vectors, also work on
;; strings.

(length "Foo") ; => 3
(reverse "Foo") ; => "ooF"
(aref "Foo" 2) ; => #\o

;; Characters objects do not need to be quoted because they evaluate
;; to themselves, just as numbers do.

#\k ; => #\k
(type-of #\k) ; => STANDARD-CHAR

;; HASH TABLES
;; -----------

;; - Same functionality as an association list.
;; - Are implemented using special hashing algorithms
;; - Much faster
;; - Are implemented using vectors rather than cons cell chains

(setf h (make-hash-table))

(type-of h) ; => HASH-TABLE

;; GETHASH looks up a key in a hash table:

(setf (gethash 'john h)
      '(attorney (16 maple drive)))

(setf (gethash 'mari h)
      '(physician (23 cedar court)))

(gethash 'john h) ; => (ATTOREY (16 MAPLE DRIVE)), T
(gethash 'bil h) ; => NIL, NIL

(describe h)
(inspect h)

(room) ; => prints a summary of Lisp's current memory usage.

;; COERCE convert a sequence from one type to another

(coerce "foo" 'list) ; => (#\f #\o #\o)
(coerce '(#\f #\o #\o) 'string) ; => "foo"
(coerce '(foo bar baz) 'vector) ; => #(FOO BAR BAZ)

;; MAP is mapping function, that works on sequences of any type.

(map 'list #'+
     '(1 2 3 4)
     '#(1 2 3 4)) ; => (2 4 6 8)

(map 'vector #'+
     '(1 2 3 4)
     '#(1 2 3 4)) ; => #(2 4 6 8)

(map 'list #'list
     '(a b c)
     '(x y z)
     '#(1 2 3)) ; => ((A X 1) (B Y 2) (C Z 3))

;; If MAP is given NIL as a first argument, it returns NIL instead of
;; constructing a sequence from the results of the mapping. This is useful if you
;; want to apply a function to every element of a sequence only for its side effect.

(map nil #'print "a b")

;; #\a
;; #\
;; #\b

