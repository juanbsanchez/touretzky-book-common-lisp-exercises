;; INPUT / OUTPUT

;; Format T function normally returns NIL, but as a side effect it causes things to be written to display or to a file.

;; Thee first argument to FORMAT should be the symbol T when we want to write to the display.

(format t "Hi, mom!")
;; Hi, mom!
;; NIL

(format t "Fruit flies ~%~%like banananas.")
;; Fruit flies

;; like bananas.
;; NIL


;; The ~& directive tells FORMAT to move to a new line unless it knows it is already at the beginning of a new line.

(format t "~&Mary had a little bat.")
;; Mary had a little bat.
;; NIL

;; Directive ~S inserts the printed representation of a Lisp object.

(format t "From ~S to ~S in ~S minutes!"
	'boston '(new york) 55)

;; From Boston to (NEW YORK) in 55 minutes!
;; NIL

(defun square-talk (n)
  (format t "~&~S squared is ~S" n (* n n)))

(square-talk 10)
;; 10 squared is 100
;; NIL

(mapcar #'square-talk '(1 2 3 4 5))
;; 1 squared is 1
;; 2 squared is 4
;; 3 squared is 9
;; 4 squared is 16
;; 5 squared is 25
;; (NIL NIL NIL NIL NIL)

;; The ~A directive prints an object without using escape characters.

(defun test (x)
  (format t "~&With escape characters: ~S" x)
  (format t "~&Without escape characters: ~A" x))

;; > (test "Hi, mom")
;; With escape characters: "Hi, mom"
;; Without escape characters: Hi, mom
;; NIL

;; 9.1. Write a function to print the following saying on the display: ‘‘There
;; are old pilots, and there are bold pilots, but there are no old bold
;; pilots.’’ Your function should break up the quotation into several lines.

(defun print-sentece ()
  (format t "~&There are old pilots,
~&and there are bold pilots, 
~&but there are no old bold pilots"))

;; 9.2. Write a recursive function DRAW-LINE that draws a line of a specified
;; length by doing (FORMAT T "*") the correct number of times.
;; (DRAW-LINE 10) should produce *********

(defun draw-line (n)
  (cond ((zerop n) (format t "~%"))
	(t (format t "*") (draw-line (- n 1)))))


;; 9.3. Write a recursive function DRAW-BOX that calls DRAW-LINE
;; repeatedly to draw a box of specified dimensions. (DRAW-BOX 10 4)
;; should produce
;; **********
;; **********
;; **********
;; **********

(defun draw-box (width height)
  (cond ((zerop height) nil)
	(t (draw-line width)
	   (draw-box width (- height 1)))))

;; 9.4. Write a recursive function NINETY-NINE-BOTTLES that sings the
;; well-known song ‘‘Ninety-nine Bottles of Beer on the Wall.’’ The first
;; verse of this song is
;; 99 bottles of beer on the wall,
;; 99 bottles of beer!
;; Take one down,
;; Pass it around,
;; 98 bottles of beer on the wall.

;; NINETY-NINE-BOTTLES should take a number N as input and start
;; counting from N down to zero. (This is so you can run it on three
;; bottles instead of all ninety nine.) Your function should also leave a
;; blank line between each verse, and say something appropriate when it
;; runs out of beer.

(defun ninety-nine-bottles (n)
  (cond ((zerop n)
	 (format t "~&Awww, no more beer!"))
	(t (do-verse n)
	   (ninety-nine-bottles (- n 1)))))

(defun do-verse (n)
  (format t
	  "~&~S bottles of beer on the wall, ~%" n)
  (format t "~S bottles of beer!~%" n)
  (format t "Take one down, ~%Pass it around,~%")
  (format t
	  "~S bottles of beer on the wall. ~%~%"
	  (- n 1)))


;; 9.5. Part of any tic-tac-toe playing program is a function to display the
;; board. Write a function PRINT-BOARD that takes a list of nine
;; elements as input. Each element will be an X, an O, or NIL. PRINTBOARD should display the corresponding board. For example,
;; (PRINT-BOARD ’(X O O NIL X NIL O NIL X)) should print:

;; X | O | O
;; -----------
;;   | X |
;; -----------
;; O |   | X

(defun print-board (b)
  (let ((b2 (sublis '((x . "x")
		      (o . "O")
		      (nil . " "))
		     b)))
    (format t "~&")
    (print-line b2)
    (format t "-----------~%")
    (print-line (nthcdr 3 b2))
    (format t "-----------~%")
    (print-line (nthcdr 6 b2))))

(defun print-line (line)
  (format t " ~A | ~A | ~A~%"
	  (first line)
	  (second line)
	  (third line)))

;;READ is a function that reads one Lisp object (a number, symbol, list, or
;; whatever) from the keyboard and returns that object as its value. The object
;; does not have to be quoted because it will not be evaluated. By placing calls
;; to READ inside a function, we can make the computer read data from the
;; keyboard under program control. Here are some examples. User type-in in
;; response to READ is underlined.

(defun my-square ()
 (format t "Please type in a number: ")
  (let ((x (read)))
   (format t "The number ~S squared is ~S.~%"
	  x (* x x))))

;; > (my-square)
;; Please type in a number: 7
;; The number 7 squared is 49.
;; NIL

;; 9.6. Write a function to compute an hourly worker’s gross pay given an
;; hourly wage in dollars and the number of hours he or she worked.
;; Your function should prompt for each input it needs by printing a
;; message in English. It should display its answer in English as well.

(defun compute-pay ()
  (format t "~&What is the hourly wage? ")
  (let ((wage (read)))
    (format t "~&How many hours workded? ")
    (let ((hours (read)))
      (format t
	      "~&The worker earned ~S dollars."
	      (* wage hours)))))

;; 9.7. The COOKIE-MONSTER function keeps reading data from the
;; terminal until it reads the symbol COOKIE. Write COOKIEMONSTER. Here is a sample interaction:

(defun cookie-monster ()
  (format t "~&~%Give me a cookie!!! ~&Cookie? ")
  (let ((x (read)))
    (cond ((equal x 'cookie)
	   (format t "Thank you!... Munch munch munch... BURP"))
	  (t
	   (format t "No want ~S...~%" x)
	   (cookie-monster)))))

;; The YES-OR-NO-P function takes a format control string as input and asks
;; the user a yes or no question. The user must respond by typing ‘‘yes,’’ in
;; which case the function returns T, or ‘‘no,’’ in which case it returns NIL.

(defun riddle ()
  (if (yes-or-no-p
       "Do you seek Zen enlightenment? ")
      (format t "Then do not ask for it!")
      (format t "You have found it. ")))

;; There is also a shorter form of this function, called Y-OR-N-P, that only
;; requires the user to type ‘‘y’’ or ‘‘n’’ in response.

;; The WITH-OPEN-FILE macro provides a convenient way to read data from a
;; file. Its syntax is:
;; (WITH-OPEN-FILE (var pathname)
;; body)

(defun get-tree-data ()
 (with-open-file (stream "/usr/dst/timber.dat")
   (let* ((tree-loc (read stream))
	  (tree-table (read stream))
	  (num-trees (read stream)))
     (format t "~&There are ~S trees on ~S."
	  num-trees tree-loc)
     (format t "~&They are: ~S" tree-table))))

;; We can also use WITH-OPEN-FILE to open files for output by passing it the
;; special keyword argument :DIRECTION :OUTPUT.
;; The stream that WITHOPEN-FILE creates can then be used in place of the usual T as a first
;; argument to FORMAT.

(defun save-tree-data (tree-loc tree-table
		       num-trees)
  (with-open-file (stream "/usr/dst/timber.newdat"
			  :direction :output)
    (format stream "~S~%" tree-loc)
    (format stream "~S~%" tree-table)
    (format stream "~S~%" num-trees)))

;; (save-tree-data
;; "The West Ridge"
;; ’((45 redwood) (22 oak) (43 maple))
;; 110)
;; NIL



;; In this exercise we will write a program for producing a graph of an arbitrary
;; function. The program will prompt for a function name F and then plot y =
;; F(x) for a specified range of x values. Here is an example of how the program
;; works:

;; > (make-graph)
;; Function to graph? square
;; Starting x value? -7
;; Ending x value? 7
;; Plotting string? "****"

;; 9.10. As you write each of the following functions, test it by calling it from
;; top level with appropriate inputs before proceeding on to the next
;; function.

;; a. Write a recursive function SPACE-OVER that takes a number N as
;; input and moves the cursor to the right by printing N spaces, one at a
;; time. SPACE should print ‘‘Error!’’ if N is negative. Test it by
;; using the function TEST. Try (TEST 5) and (TEST −5).
;; (defun test (n)
;; (format t "~%>>>")
;; (space-over n)
;; (format t "<<<"))

(defun space-over (n)
  (cond ((plusp n)
	 (format t " ")
	 (space-over (- n 1)))
	((zerop n) nil)
	(t (format t "Error!"))))

;; b. Write a function PLOT-ONE-POINT that takes two inputs,
;; PLOTTING-STRING and Y-VAL, prints PLOTTING-STRING
;; (without the quotes) in column Y-VAL, and then moves to a new
;; line. The leftmost column is numbered zero.

(defun plot-one-point (plotting-string y-val)
  (space-over y-val)
  (format t "~A~%" plotting-string))

;; c. Write a function PLOT-POINTS that takes a string and a list of y
;; values as input and plots them. (PLOT-POINTS "< >" ’(4 6 8 10 8 6
;; 4)) should print

;;  < >
;;     < >
;;       < >
;;         < >
;;      < >
;;   < >
;; < >


(defun plot-points (plotting-string y-vals)
  (mapcar
   #'(lambda (y)
   (plot-one-point plotting-string y))
   y-vals))

;; d. Write a function GENERATE that takes two numbers M and N as
;; input and returns a list of the integers from M to N. (GENERATE -3
;; 3) should return (-3 -2 -1 0 1 2 3).

(defun generate (m n)
  (cond ((equal m n) (list n))
	(t (cons m (generate (+ m 1) n)))))


;; e. Write the MAKE-GRAPH function. MAKE-GRAPH should
;; prompt for the values of FUNC, START, END, and PLOTTINGSTRING, and then graph the function. Note: You can pass FUNC
;; as an input to MAPCAR to generate the list of y values for the
;; function. What will the second input to MAPCAR be?

(defun make-graph ()
  (let* ((func
	   (prompt-for "Function to graph? "))
	 (start
	   (prompt-for "Starting x value? "))
	 (end (prompt-for "Ending x value? "))
	 (plotting-string
	   (prompt-for "Plotting string? ")))
    (plot-points plotting-string
		 (mapcar func (generate start end)))
    t))

;; f. Define the SQUARE function and graph it over the range -7 to 7.
;; Use your first name as the plotting symbol.

(defun prompt-for (prompt-string)
  (format t "~A" prompt-string)
  (read))

;; Here is an example of a program that reads an arbitrary file of Lisp objects,
;; tells how many objects were read, and returns a list of them. It uses the cons
;; cell ($EOF$) as its special end-of-file value, but any freshly generated cons
;; cell will do, since only the cell’s address is important, not its contents.

(defun read-my-file ()
  (with-open-file (stream "/usr/dst/sample-file")
    (let ((contents
	   (read-all-objects stream (list ´$efo$))))
      (format t "~&Read ~S objects from the file."
	      (length contents))
      contents)))

(defun read-all-objects (stream eof-indicator)
  (let ((result (read stream nil eof-indicator)))
    (if (eq result eof-indicator)
	nil
	(cons result (read-all-objects stream)))))

;; Suppose our sample file contains the following lines:
;; 35 cat (moose
;; meat) 98.6 "Frozen yogurt"
;; 3.14159

;; This program would produce the following result:

;; > (read-my-file)
;; Read 6 objects from the file.
;; (35 CAT (MOOSE MEAT) 98.6 "Frozen yogurt" 3.14159)

;; EXERCISES

;; 9.11. Write a function DOT-PRIN1 that takes a list as input and prints it in
;; dot notation. DOT-PRIN1 will print parentheses by (FORMAT T "(")
;; and (FORMAT T ")"), and dots by (FORMAT T " . "), and will
;; call itself recursively to print lists within lists. DOT-PRIN1 should
;; return NIL as its result. Try (DOT-PRIN1 ’(A (B) C)) and see if your
;; output matches the result in the table above. Then try (DOT-PRIN1
;; ’((((A))))).

(defun dot-prin1 (x)
  (cond ((atom x) (format t "~S" x))
	(t (format t " (")
	   (dot-prin1 (car x))
	   (format t " . ")
	   (dot-prin1 (cdr x))
	   (format t ")"))))

(dot-prin1 '(a b c)) ; => (A . (B . (C . NIL)))

;; 10.5. Rewrite the following ugly function to use good Lisp style.
(defun ugly (x y)
  (when (> x y)
    (setf temp y)
    (setf y x)
    (setf x temp))
  (setf avg (/ (+ x y) 2.0))
  (setf pct (* 100 (/ avg y)))
  (list ’average avg ’is
	 pct ’percent ’of ’max y))

;; (ugly 20 2) ⇒
;; (average 11.0 is 55.0 percent of max 20)

(defun pretty (x y)
  (let* ((biggest (max x y))
	 (smallest (min x y))
	 (avg (/ (+ x y) 2.0))
	 (pct (* 100 (/ avg biggest))))
    (list 'average avg 'is pct 'percent)))

