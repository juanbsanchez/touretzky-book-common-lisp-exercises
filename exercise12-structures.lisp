
;; 12. STRUCTURES AND THE TYPE SYSTEM
;; ----------------------------------

;; TYPEP predicate returns true if an object is of the specified type.

(typep 3 'number) ; => T
(typep 'foo 'symbol) ; => T
(typep "foo" 'string) ; => T
(typep 3.0 'float) ; => T
(typep 3 'integer) ; => T
(typep '(a b c) 'list) ; => T

(typep nil 'null) ; => T
(typep nil 'list) ; => T
(typep nil 'symbol) ; => T

;; TYPE-OF returns a type specifier for an object.

(type-of 'foo) ; => SYMBOL
(type-of 3.5) ; => SINGLE-FLOAT
(type-of '(a b c)) ; => CONS
(type-of "foo") ; => (SIMPLE-ARRAY CHARACTER (3))

;; STRUCTURES are programmer-defined Lisp oebjects with an arbitrary numbers o named components.
;; Structure types automatically become part of the Lisp type hierarchy.
;; The DEFSTRUCT macro defines new structures:

(defstruct starship
  (name nil)
  (speed 0)
  (condition 'green)
  (shields 'down))

;; The DEFSTRUCT macro function also does several other things. It
;; defines a constructor function MAKE-STARSHIP for creating new structures
;; of this type. When a new starship is created, the name component will default
;; to NIL, the speed to zero, the condition to GREEN, and the shields to DOWN.

(setf s1 (make-starship))

;; #S(STARSHIP NAME NIL
;; SPEED 0
;; CONDITION GREEN
;; SHIELDS DOWN)

;; The #S notation is the standard way to display structures in Common Lisp.
;; The list following the #S contains the type of the structure followed by an
;; alternating sequence of component names and values. Do not be misled by the
;; use of parentheses in #S notation: Structures are not lists. Ordinary list
;; operations like CAR and CDR will not work on structures.

;; s1 ⇒ #s(starship name nil
;; speed 0
;; condition green
;; shields down)

;; Although new instances are usually created by calling the constructor
;; function MAKE-STARSHIP, it is also possible to type in STARSHIP objects
;; directly to the read-eval-print loop, using #S notation. Notice that the
;; structure must be quoted to prevent its evaluation.

(setf s2 '#s(starship speed (warp 3)
		      condition red
		      shields up))


;; When a new structure is defined, DEFSTRUCT creates accessor functions for
;; each of its components. For example, it creates a STARSHIP-SPEED
;; accessor for retrieving the SPEED component of a starship.

(starship-speed s2) ; => (warp 3)
(starshp-shields s2) ; => up

(setf (starship-name s1) "Enterprise")
(incf (starship-speed s1))

s1
;; #S(STARSHIP NAME "Enterprise"
;;             SPEED 1
;;             CONDITION GREEN
;;             SHIELDS DOWN)

;; Using these accessor functions, we can easily write our own functions to
;; manipulate structures in interesting ways. For example, the ALERT function
;; below causes a starship to raise its shields, and in addition raises the condition
;; level to be at least YELLOW.

(defun alert (strship)
  (setf (starship-shields strship) 'up)
  (if (equal (starship-condition strship) 'green)
      (setf (starship-condition strship) 'yellow))
  'shields-raised)

(alert s1) ; => SHIELDS-RAISED
s1 ; ⇒ #s(starship name "Enterprise"
;                  speed 1
;                  condition yellow
;                  shields up)



;; Creating new structure specifying different values.

(setf s3 (make-starship :name "Relian"
			:shields 'damaged))

;; #S(STARSHIP NAME "Reliant"
;;             SPEED 0
;;             CONDITION GREEN
;;             SHIELDS DAMAGED)


;; Rebuild structure using the redefined constructor function make-starship:

(defstruct starship
  (captain nil)
  (name nil)
  (shields 'down)
  (condition 'green)
  (speed 0))

(setf s3 (make-starship :captain "Benson"
			:name "Realian"
			:shields 'damaged))




;; DEFSTRUCT also automatically defines a constructor function for the type (such as MAKE-STARSHIP) and a type predicate (such as STARSHIP-P).

(starship-p s3) ; => T

(type-of (make-starship)) ; => STARSHIP

