
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




;; DEFSTRUCT also automatically defines a constructor function for the type
;; (such as MAKE-STARSHIP) and a type predicate (such as STARSHIP-P).

(starship-p s3) ; => T

(type-of (make-starship)) ; => STARSHIP

;; DESCRIBE is a function that takes any kind of Lisp object as input and prints
;; an informative description of it.

(describe s1)

;; NAME = "Enterprise"
;; SPEED = 1
;; CONDITION = YELLOW
;; SHIELDS = UP

;; Inspect works similar:

(inspect s1)

;; 0. NAME: "Enterprise"
;; 1. SPEED: 1
;; 2. CONDITION: YELLOW
;; 3. SHIELDS: UP
;; >

;; KEYBOARD EXERCISE
;; -----------------

;; In this keyboard exercise we will implement a discrimination net.
;; Discrimination nets are networks of yes and no questions used for problemsolving tasks, such as diagnosing automotive engine trouble. Here are two
;; examples of dialogs with a car diagnosis net:
;; > (run)
;; Does the engine turn over? no
;; Do you hear any sound when you turn the key? no
;; Is the battery voltage low? no
;; Are the battery cables dirty or loose? yes
;; Clean the cables and tighten the connections.
;; NIL
;; > (run)
;; Does the engine turn over? yes
;; Does the engine run for any length of time? no
;; Is there gas in the tank? no
;; Fill the tank and try starting the engine again.
;; NIL
;; Figure 12-2 shows a portion of the discrimination net that generated this
;; dialog. The net consists of a series of nodes. Each node has a name (a
;; symbol), an associated question (a string), a ‘‘yes’’ action, and a ‘‘no’’ action.
;; The yes and no actions may either be the names of other nodes to go to, or
;; they may be strings that give the program’s diagnosis. Since in the latter case
;; there is no new node to which to go, the program stops after displaying the
;; string.
;; Figure 12-3 shows how the net will be created. Note that the tree of
;; questions is incomplete. If we follow certain paths, we may end up trying to
;; go to a node that hasn’t been defined yet, as shown in the following. In that
;; case the program just prints a message and stops.

;; (run)
;; Does the engine turn over? yes
;; Will the engine run for any period of time? yes
;; Node ENGINE-WILL-RUN-BRIEFLY not yet defined.
;; NIL

;; 12.4. In this exercise you will create a discrimination net for automotive
;; diagnosis that mimics the behavior of the system shown in the
;; preceding pages.

;; a. Write a DEFSTRUCT for a structure called NODE, with four
;; components called NAME, QUESTION, YES-CASE, and NO-CASE.

(defstruct node
  name
  question
  yes-case
  no-case)


;; b. Define a global variable *NODE-LIST* that will hold all the nodes
;; in the discrimination net. Write a function INIT that initializes the
;; network by setting *NODE-LIST* to NIL.

(setf *node-list* nil)

(defun init ()
  (setf *node-list* nil)
  'initialized)


(add-node 'start
	  "Does the engine turn over?"
	  'engine-turns-over
	  'engine-wont-turn-over)

(add-node 'engine-turns-over
	  "Will the engine run for any period of time?"
	  'engine-will-run-briefly
	  'engine-wont-run)

(add-node 'engine-wont-run
	  "Is there gas in the tank?"
	  'gas-in-tank
	  "Fill the tank and try starting the engine again.")

(add-node 'engine-wont-turn-over
	  "Do you hear any sound when you turn the key?"
	  'sound-when-turn-key
	  'no-sound-when-turn-key)

(add-node 'no-sound-when-turn-key
	  "Is the battery voltage low?"
	  "Replace the battery"
	  'battery-voltage-ok)

(add-node 'battery-voltage-ok
	  "Are the battery cables dirty or loose?"
	  "Clean the cables and tighten the connections."
	  'battery-cables-good)

;; c. Write ADD-NODE. It should return the name of the node it added.

(defun add-node (name question yes-case no-case)
  (push (make-node :name name
		   :question question
		   :yes-case yes-case
		   :no-case no-case)
	*node-list*))


;; d. Write FIND-NODE, which takes a node name as input and returns
;; the node if it appears in *NODE-LIST*, or NIL if it doesn’t.

(defun find-node (x)
  (find-if #'(lambda (node)
	       (equal (node-name node) x))
	   *node-list*))

;; e. Write PROCESS-NODE. It takes a node name as input. If it can’t
;; find the node, it prints a message that the node hasn’t been defined
;; yet, and returns NIL. Otherwise it asks the user the question
;; associated with that node, and then returns the node’s yes action or
;; no action depending on how the user responds.

(defun process-node (name)
  (let ((nd (find-node name)))
    (if nd
	(if (y-or-n-p "~&~A "
		      (node-question nd))
	    (node-yes-case nd)
	    (node-no-case nd))
	(format t
		"~&Node ~S not yet defined." name))))

;; f. Write the function RUN. It maintains a local variable named
;; CURRENT-NODE, whose initial value is START. It loops, calling
;; PROCESS-NODE to process the current node, and storing the value
;; returned by PROCESS-NODE back into CURRENT-NODE. If the
;; value returned is a string, the function prints the string and stops. If
;; the value returned is NIL, it also stops

(defun run ()
  (do ((current-node 'start
		     (process-node current-node)))
      ((null current-node) nil)
    (cond ((stringp current-node)
	   (format t "~&~A" current-node)
	   (return nil)))))

