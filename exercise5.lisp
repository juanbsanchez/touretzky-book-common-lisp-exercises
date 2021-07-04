;;; EXERCISE 5. VARIABLES AND SIDE EFFECTS.
;;; ---------------------------------------

;;; 5.1. Rewrite function POOR-STYLE to create a new local variable Q using
;;; LET, instead of using SETF to change P. Call your new function
;;; GOOD-STYLE.

(defun good-style (p)
  (let ((q (+ p 5)))
    (list 'result 'is q)))

(good-style 8) ;; => (result is 13)


;;; 5.6. This keyboard exercise is about dice. We will start with a function to
;;; throw one die and end up with a program to play craps. Be sure to
;;; include a documentation string for each function you write.

;;; a. Write a function THROW-DIE that returns a random number from 1
;;; to 6, inclusive. Remember that (RANDOM 6) will pick numbers
;;; from 0 to 5. THROW-DIE doesn’t need any inputs, so its argument
;;; list should be NIL.

(defun throw-die ()
  (+ 1 (random 6)))

;;; b. Write a function THROW-DICE that throws two dice and returns a
;;; list of two numbers: the value of the first die and the value of the
;;; second. We’ll call this list a ‘‘throw.’’
;;; For example, (THROWDICE) might return the throw (3 5),
;;; indicating that the first die wasa 3 and the second a 5.

(defun throw-dice ()
  (list (throw-die) (throw-die)))

;;; c. Throwing two ones is called ‘‘snake eyes’’; two sixes is called
;;; ‘‘boxcars.’’ Write predicates SNAKE-EYES-P and BOXCARS-P
;;; that take a throw as input and return T if the throw is equal to (1 1)
;;; or (6 6), respectively.

(defun snake-eyes-p (throw)
  (equal throw '(1 1)))

(defun boxcars-p (throw)
  (equal throw '(6 6)))

;;; In playing craps, the first throw of the dice is crucial. A throw of 7
;;; or 11 is an instant win. A throw of 2, 3, or 12 is an instant loss
;;; (American casino rules). Write predicates INSTANT-WIN-P and
;;; INSTANT-LOSS-P to detect these conditions. Each should take a
;;; throw as input.

(defun throw-value (throw)
  "this is a helping function used by several of the functions that follow"
  (+ (first throw) (second throw)))

(defun instant-win-p (throw)
  (member (throw-value throw) '(7 11)))

(defun instant-loss-p (throw)
  (member (throw-value throw) '(2 3 12)))

;;; Write a function SAY-THROW that takes a throw as input and
;;; returns either the sum of the two dice or the symbol SNAKE-EYES
;;; or BOXCARS if the sum is 2 or 12. (SAY-THROW ’(3 4)) should
;;; return 7. (SAY-THROW ’(6 6)) should return BOXCARS.

(defun say-throw (throw)
  (cond ((snake-eyes-p throw) 'snake-eyes)
        ((boxcars-p throw) 'boxcars)
        (t (throw-value throw))))


;;; If you don’t win or lose on the first throw of the dice, the value you
;;; threw becomes your ‘‘point,’’ which will be explained shortly.
;;; Write a function (CRAPS) that produces the following sort of
;;; behavior. Your solution should make use of the functions you wrote
;;; in previous steps


(defun craps ()
  (let ((throw (throw-dice)))
    (append
     (list 'throw (first throw)
           'and (second throw)
           '--
           (say-throw throw)
           '--)
     (cond ((instant-win-p throw) '(you win))
           ((instant-loss-p throw) '(you lose))
           (t (list 'your 'point 'is
                    (throw-value throw)))))))


;; Once a point has been established, you continue throwing the dice
;; until you either win by making the point again or lose by throwing a
;; 7. Write the function TRY-FOR-POINT that simulates this part of
;; the game, as follows:

(defun try-for-point (point)
  (let* ((throw (throw-dice))
         (val (throw-value throw)))
    (append
     (list 'throw (first throw)
           'and (second throw)
           '--
           (say-throw throw)
           '--)
     (cond ((equal val point) '(you win))
           ((equal val 7) '(you lose))
           (t '(throw again))))))




