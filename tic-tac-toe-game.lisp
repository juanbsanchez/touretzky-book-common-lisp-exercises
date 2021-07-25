;; In this section we will write our first large program: a program that not only plays tic-tac-toe, but also explains the strategy behind each move.
;; When writing a program this complex, it pays to take a few minutes at the outset to think about the overall design,
;; particularly the data structures that will be used.
;; Let’s start by developing a representation for the board. We will number the squares on the tic-tac-toe board this way:

;; 1 | 2 | 3
;; -----------
;; 4 | 5 | 6
;; -----------
;; 7 | 8 | 9

;; We will represent a board as a list consisting of the symbol BOARD
;; followed by nine numbers that describe the contents of each position. A zero means the position is empty.
;; A one means it is filled by an O; a ten means it is
;; filled by an X. The function MAKE-BOARD creates a new tic-tac-toe board:

(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

;; Notice that if B is a variable holding a tic-tac-toe board, position one of the
;; board can be accessed by writing (NTH 1 B), position two by (NTH 2 B), and
;; so on. (NTH 0 B) returns the symbol BOARD.
;; Now let’s write functions to print out the board. CONVERT-TO-LETTER
;; converts a zero, one, or ten to a space, an O, or an X, respectively. It is called
;; by PRINT-ROW, which prints out one row of the board. PRINT-ROW is in
;; turn called by PRINT-BOARD.

(defun convert-to-letter (v)
  (cond ((equal v 1) "O")
	((equal v 10) "X")
	(t " ")))

(defun print-row (x y z)
  (format t "~&  ~A | ~A | ~A"
	  (convert-to-letter x)
	  (convert-to-letter y)
	  (convert-to-letter z)))

(defun print-board (board)
  (format t "~%")
  (print-row
   (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~& -----------")
  (print-row
	 (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~& -----------")
  (print-row
	 (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%"))

;; We can make a move by destructively changing one of the board positions
;; from a zero to a one (for O) or a ten (for X). The variable PLAYER in
;; MAKE-MOVE will be either one or ten, depending on who’s moving.

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

;; Let’s make a sample board to test out these functions before proceeding
;; further. We’ll define variables *COMPUTER* and *OPPONENT* to hold
;; the values ten and one (X and O), respectively, because this will make the
;; example clearer

(setf *computer* 10)
(setf *opponent* 1)

(make-move *opponent* 3 b)
;; (board 0 0 1 0 0 0 0 0 0)

(make-move *computer* 5 b)
;; (board 0 0 1 0 10 0 0 0 0)

(print-board b)

;;   |   | O
;; -----------
;;   | X |
;; -----------
;;   |   |

;; For the program to select the best move, it must have some way of
;; analyzing the board configuration. This is easy for tic-tac-toe. There are only
;; eight ways to make three-in-a-row: three horizontally, three vertically, and two
;; diagonally. We’ll call each of these combinations a ‘‘triplet.’’ We’ll store a
;; list of all eight triplets in a global variable *TRIPLETS*.

(setf *triplets*
      '((1 2 3) (4 5 6) (7 8 9)         ;Horizontal triplets.
		(1 4 7) (2 5 8) (3 6 9) ;Vertical triplets.
		(1 5 9) (3 5 7)))       ;Diagonal triplets.



;; Now we can write a SUM-TRIPLET function to return the sum of the
;; numbers in the board positions specified by that triplet. For example, the right
;; diagonal triplet is (3 5 7). The sum of elements three, five, and seven of board
;; B is eleven, indicating that there is one O, one X, and one blank (in some
;; unspecified order) on that diagonal. If the sum had been twenty-one, there
;; would be two Xs and one O; a sum of twelve would indicate one X and two
;; Os, and so on.

(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(sum-triplet b '(3 5 7)) ;Left diagonal triplet.
;; 11

(sum-triplet b '(2 5 8)) ;Middle vertical triplet.
;; 10

(sum-triplet b '(7 8 9)) ;Bottom horizontal triplet.
;; 1

;; To fully analyze a board we have to look at all the sums. The function
;; COMPUTE-SUMS returns a list of all eight sums

(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
	      (sum-triplet board triplet))
	  *triplets*))

(compute-sums b) ; => (1 10 0 0 10 1 10 11)

;; Notice that if player O ever gets three in a row, one of the eight sums will
;; be three. Similarly, if player X manages to get three in a row, one of the eight
;; sums will be 30. We can write a predicate to check for this condition:

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
	(member (* 3 *opponent*) sums))))


;; We’ll return to the subject of board analysis later. Let’s look now at the
;; basic framework for playing the game. The function PLAY-ONE-GAME
;; offers the user the choice to go first, and then calls either COMPUTERMOVE or OPPONENT-MOVE as appropriate, passing a new, empty board as
;; input.

(defun play-one-game ()
  (if (y-or-n-p "Would you like go to first?"
		(opponent-move (make-board))
		(computer-move (make-board)))))

;; The OPPONENT-MOVE function asks the opponent to type in a move and
;; checks that the move is legal. It then updates the board and calls
;; COMPUTER-MOVE. But there are two special cases where we should not
;; call COMPUTER-MOVE. First, if the opponent’s move makes a three-in-arow, the opponent has won and the game is over. Second, if there are no
;; empty spaces left on the board, the game has ended in a tie. We assume that
;; the opponent is O and the computer is X.

(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
	 (new-board (make-move
		     *opponent*
		     pos
		     board)))
    (print-board new-board)
    (cond ((winner-p new-board)
	   (format t "~&You win!"))
	  ((board-full-p new-board)
	   (format t "~&Tie game."))
	  (t (computer-move new-board)))))

;; A legal move is an integer between one and nine such that the
;; corresponding board position is empty. READ-A-LEGAL-MOVE reads a
;; Lisp object and checks whether it’s a legal move. If not, the function calls
;; itself to read another move. Notice that the first two COND clauses each
;; contain a test and two consequents. If the test is true, both consequents are
;; evaluated, and the value of the last one (the recursive call) is returned.

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
		     (<= 1 pos 9)))
	   (format t "~&Invalid input.")
	   (read-a-legal-move board))
	  ((not (zerop (nth pos board)))
	   (format t
		   "~&That space is already occupied.")
	   (read-a-legal-move board))
	  (t pos))))

;; The BOARD-FULL-P predicate is called by OPPONENT-MOVE to test if
;; there are no more empty spaces left on the board:

(defun board-full-p (board)
  (not (member 0 board)))

;; The COMPUTER-MOVE function is similar to OPPONENT-MOVE,
;; except the player is X instead of O, and instead of reading a move from the
;; keyboard, we will call CHOOSE-BEST-MOVE. This function returns a list of
;; two elements. The first element is the position in which to place an X. The
;; second element is a string explaining the strategy behind the move.

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
	 (pos (first best-move))
	 (strategy (second best-move))
	 (new-board (make-move
		     *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board)
	   (format t "~&I win!"))
	  ((board-full-p new-board)
	   (format t "~&Tie game."))
	  (t (opponent-move new-board)))))

;; Now we’re almost ready to play our first game. Our first version of
;; CHOOSE-BEST-MOVE will have only one strategy: Pick a legal move at
;; random. The function RANDOM-MOVE-STRATEGY returns a list whose
;; first element is the move, and whose second element is a string explaining the
;; strategy behind the move. The function PICK-RANDOM-EMPTY-POSITION picks a random number from one to nine. If that board position is
;; empty, it returns that move. Otherwise, it calls itself recursively to try another
;; random number.

(defun choose-best-move (board) ;First version.
  (random-move-strategy board))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
	"random move"))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
	pos
	(pick-random-empty-position board))))

;; You can try playing a few games with the program to see how it feels.
;; Pretty soon you’ll notice that the random move strategy isn’t very good near
;; the end of the game; sometimes it causes the program to make moves that are
;; downwright stupid. Consider this example:

(setf b '(board 10 10 0
	  0  0 0
	  1 1 0))

(print-board b)

;;  X | X |
;; -----------
;;    |   |
;; -----------
;;    | O | O

(computer-move b)

;; My move: 4
;; My strategy: random move

;; X | X |
;; -----------
;; X |   |
;; -----------
;; O | O |

;; Your move: 9

;; X | X |
;; -----------
;; X |   |
;; -----------
;; O | O | O

;; You win!
;; NIL

;; The computer already had two in a row; it could have won by putting an X
;; in position three. But instead it picked a move at random and ended up putting
;; an X in position four, which did no good at all because that vertical triplet was
;; already blocked by the O at position nine.
;; To make our program smarter, we can program it to look for two-in-a-row
;; situations. If there are two Xs in a row, it should fill in the third X to win the
;; game. Otherwise, if there are two Os in a row, it should put an X there to
;; block the opponent from winning.

(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board
			   (* 2 *computer*))))
    (and pos (list pos "make three in a row"))))

(defun block-opponent-win (board)
  (let ((pos (win-or-block board
			   (* 2 *opponent*))))
    (and pos (list pos "block opponent"))))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if
		  #'(lambda (trip)
		      (equal (sum-triplet board
					  trip)
			     target-sum))
		  *triplets*)))
    (when triplet
      (find-empty-position board triplet))))

(defun find-empty-position (board squares)
  (find-if #'(lambda (pos)
	       (zerop (nth pos board)))
	     squares))
;; Both MAKE-THREE-IN-A-ROW and BLOCK-OPPONENT return NIL if
;; they cannot find a move that fits their respective strategies. Now we need to
;; revise CHOOSE-BEST-MOVE to prefer these two more clever strategies to
;; the random move strategy. We introduce an OR into the body of CHOOSEBEST-MOVE so that it will try its strategies one at a time until one of them
;; returns a non-NIL move.

(defun choose-best-move (board)   ;Second version.
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (random-move-strategy board)))

;; This new strategy makes for a more interesting game. The computer will
;; defend itself when it is obvious the opponent is about to win, and it will take
;; advantage of the opportunity to win when it has two in a row.

;; > (play-one-game)
;; Would you like to go first? y
;; Your move: 1
;;  O |  |
;; -----------
;;    |  |
;; -----------
;; |  |

;; My move: 5
;; My strategy: random move

;;  O |  |
;; -----------
;;    | X |
;; -----------
;;    |   |

;; Your move: 2

;;  O | O |
;; -----------
;;    | X |
;; ----------
;;    |   |

;; My move: 3
;; My strategy: block opponent

;; O | O | X
;; -----------
;;   | X |
;; -----------
;;   |   |

;; Your move: 4

;; O | O | X
;; -----------
;; O | X |
;; -----------
;;   |   |

;; My move: 7
;; My strategy: make three in a row

;; O | O | X
;; -----------
;; O | X |
;; -----------
;; X |   |

;; I win!
;; NIL
