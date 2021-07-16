
;; RECURSION
;; ---------

;; Here is a recursive function ANYODDP that returns T if any element of a list of numbers id odd.
;; It returns NIL if none of them are:

(defun anyoddp (x)
  (cond ((null x) nil)
        ((oddp (first x)) t)
        (t (anyoddp (rest x)))))

;; 8.2. Show how to write ANYODDP using IF instead of COND.

(defun anyoddp (x)
  (if (null x) nil
      (if (oddp (first x)) t
          (anyoddp (rest x)))))


;; A lisp version of the factorial function

(defun fact (n)
  (cond ((zerop n) 1)
        (t (* n (fact (- n 1))))))


;; If we didn't have LENGTH, we could still count the slices recursively:

(defun count-slices (loaf)
  (cond ((null loaf) 0)
        (t (+ 1 (count-slices (rest loaf))))))


;; 3 rules for solving problems recursively:

;; 1. Know when to stop.
;; 2. Decide how to take one step
;; 3. Beak the journey down into that step plus a smaller journey

;; EXERCISE:

;; 8.4. We are going to write a function called LAUGH that takes a number as
;; input and returns a list of that many HAs. (LAUGH 3) should return
;; the list (HA HA HA). (LAUGH 0) should return a list with no HAs in
;; it, or, as the dragon might put it, ‘‘the empty laugh.’’

(defun laugh (n)
  (cond ((zerop n) nil)
        (t (cons 'ha (laugh (- n 1))))))


;; 8.5. In this exercise we are going to write a function ADD-UP to add up all
;; the numbers in a list. (ADD-UP ’(2 3 7)) should return 12. You
;; already know how to solve this problem applicatively with REDUCE;
;; now you’ll learn to solve it recursively. Before writing ADD-UP we
;; must answer three questions posed by our three rules of recursion.

(defun add-up (x)
  (cond ((null x) 0)
        (t (+ (first x) (add-up (rest x))))))


;; 8.6. Write ALL-ODDP, a recursive function that returns T if all the numbers
;; in a list are odd.

(defun all-oddp (x)
  (if (null x) t
      (if (oddp (first x))
          (all-oddp (rest x)))))

(defun all-oddp-alt (x)
  (cond ((null x) t)
        ((evenp (first x)) nil)
        (t (all-oddp-alt (rest x)))))


;; 8.7. Write a recursive version of MEMBER. Call it REC-MEMBER so you
;; don’t redefine the built-in MEMBER function.

(defun rec-member (x y)
  (cond ((null y) nil)
        ((equal x (first y)) y)
        (t (rec-member x (rest y)))))


;; 8.8. Write a recursive version of ASSOC. Call it REC-ASSOC.

(defun rec-assoc (x y)
  (cond ((null y) nil)
        ((equal x (car (first y)))
         (first y))
        (t (rec-assoc x (rest y)))))


;; 8.9. Write a recursive version of NTH. Call it REC-NTH.

(defun rec-nth (n x)
  (cond ((zerop n) (first x))
        (t (rec-nth (- n 1) (rest x)))))


;; 8.10. For x a nonnegative integer and y a positive integer, x+y equals
;; x+1+(y-1). If y is zero then x+y equals x. Use these equations to build
;; a recursive version of + called REC-PLUS out of ADD1, SUB1,
;; COND and ZEROP. You’ll have to write ADD1 and SUB1 too.

(defun add1 (n) (+ n 1))

(defun sub1 (n) (- n 1))

(defun rec-plus (x y)
  (cond ((zerop y) x)
        (t (rec-plus (add1 x) (sub1 y)))))


;; 8.14. Write the very shortest infinite recursion function you can.

(defun x () (x))


;; RECURSION TEMPLATES:
;; -------------------

;; Double-Test Tail Recursion

;; "Double-test" indicates that the recursive function has two end tests;
;; if either is true, the corresponding end value is returned instead of proceeding with the recursion.
;; When both en tests are false, we end up at the template.

;; TEMPLATE:
;; (defun func (x)
;;   (cond (end-test-1 end-value-1)
;;         (end-test-2 end-value-2)
;;         (t (func reduced-x))))

(defun anyoddp (x)
  (cond ((null x) nil) ; First test
        ((oddp (first x)) t) ; Second test
        (t (anyoddp (rest x))))) ; Tail

;; This template is said to be tail-recursive because the
;; action part of the last COND clause does not do any work after the recursive
;; call. Whatever result the recursive call produces, that is what the COND
;; returns, so that is what each parent call returns. ANYODDP is an example of
;; a tail-recursive function.


;; 8.17. Use double-test tail recursion to write FIND-FIRST-ODD, a function
;; that returns the first odd number in a list, or NIL if there are none. Start
;; by copying the recursion template values for ANYODDP; only a small
;; change is necessary to derive FIND-FIRST-ODD.

(defun find-first-odd (x)
  (cond ((null x) nil)
        ((oddp (car x)) (first x))
        (t (find-first-odd (rest x)))))


;; Single-Test Tail Recursion

;; Single-Test is used when we know the function will always find what it's looking for eventually;

;; 8.18. Use single-test tail recursion to write LAST-ELEMENT, a function that
;; returns the last element of a list. LAST-ELEMENT should recursively

;; TEMPLATE
;; (DEFUN func (X)
;;   (COND (end-test end-value)
;;         (T (func reduced-x))))

(defun last-element(x)
  (cond ((atom x) x)
        (t (last-element (rest x)))))

(defun last-element-alt (x)
  (cond ((equal (cdr x) nil) (car x))
        (t (last-element (rest x )))))

;; SINGLE TEST AUGMENING RECURSION

(DEFUN func (X)
  (COND (end-test end-value)
        (T (aug-fun aug-val
                    (func reduced-x)))))

(defun count-slices (x)
  (cond ((null x) 0)
        (t (+ 1 (count-slices (rest x))))))

;; 8.21. Write a recursive function ADD-NUMS that adds up the numbers N,
;; N−1, N−2, and so on, down to 0, and returns the result. For example,
;; (ADD-NUMS 5) should compute 5+4+3+2+1+0, which is 15.

(defun add-nums (n)
  (cond ((zerop n) 0)
        (t (+ n (add-nums (- n 1))))))



;; 8.24. Write COUNT-DOWN, a function that counts down from n using listconsing recursion. (COUNT-DOWN 5) should produce the list (5 4 3 2  1).

(defun count-down (n)
  (cond ((zerop n) nil)
        (t (cons n (count-down (- n 1))))))


;; 8.25. How could COUNT-DOWN be used to write an applicative version of
;; FACT?

(defun applic-fact (n)
  (reduce #'* (count-down n)))


;; 8.27. Write SQUARE-LIST, a recursive function that takes a list of numbers
;; as input and returns a list of their squares. (SQUARE-LIST ’(3 4 5 6))
;; should return (9 16 25 36).

(defun square-list (x)
  (cond ((null x) nil)
        (t (cons (* (car x) (car x))
                 (square-list (rest x))))))



;; 8.28. The expressions (MY-NTH 5 ’(A B C)) and (MY-NTH 1000 ’(A B C))
;; both run off the end of the list. and hence produce a NIL result. Yet the
;; second expression takes quite a bit longer to execute than the first.
;; Modify MY-NTH so that the recursion stops as soon the function runs
;; off the end of the list.

(defun my-nth (n x)
  (cond ((null x) nil)
        ((zerop n) (first x))
        (t (my-nth (- n 1) (rest x)))))


;; 8.29. Write MY-MEMBER, a recursive version of MEMBER. This function
;; will take two inputs, but you will only want to reduce one of them with
;; each successive call. The other should remain unchanged.


(defun my-member (x y)
  (cond ((null y) nil)
        ((equal (first y) x) y)
        (t (my-member x (rest y)))))


;; 8.30. Write MY-ASSOC, a recursive version of ASSOC.

(defun my-assoc (key table)
  (cond ((null table) nil)
        ((equal key (car (first table)))
         (first table))
        (t (my-assoc key (rest table)))))


;; 8.31. Suppose we want to tell as quickly as possible whether one list is
;; shorter than another. If one list has five elements and the other has a
;; million, we don’t want to have to go through all one million cons cells
;; before deciding that the second list is longer. So we must not call
;; LENGTH on the two lists. Write a recursive function COMPARE-LENGTHS that
;; takes two lists as input and returns one of the following symbols:
;; SAME-LENGTH, FIRST-IS-LONGER, or SECOND-IS-LONGER.
;; Use triple-test simultaneous recursion.
;; Hint: If x is shorter than y and both are nonempty, then (REST x) is shorter than (REST y).

(defun compare-lengths (x y)
  (cond ((and (null x) (null y)) 'same-legth)
        ((null x) 'second-is-longer)
        ((null y) 'first-is-longer)
        (t (compare-lengths (rest x)
                            (rest y)))))


;; Conditional Augmentation
;; Template:
;; (DEFUN func (X)
;;   (COND (end-test end-value)
;;         (aug-test (aug-fun aug-val
;;                            (func reduced-x))
;;                   (T (func reduced-x))))


(defun extract-symbols (x)
  (cond ((null x) nil)
        ((symbolp (first x))
         (cons (first x)
               (extract-symbols (rest x))))
        (t (extract-symbols (rest x)))))


;; 8.32. Write the function SUM-NUMERIC-ELEMENTS, which adds up all
;; the numbers in a list and ignores the non-numbers.
;; (SUM-NUMERICELEMENTS ’(3 BEARS 3 BOWLS AND 1 GIRL)) should return seven.

(defun sum-numeric-elements (x)
  (cond ((null x) 0)
        ((numberp (first x))
         (+ (first x)
            (sum-numeric-elements (rest x))))
        (t (sum-numeric-elements (rest x)))))


;; 8.33. Write MY-REMOVE, a recursive version of the REMOVE function.

(defun my-remove (x y)
  (cond ((null y) nil)
        ((equal x (first y))
         (my-remove x (rest y)))
        (t (cons x (my-remove x (rest y))))))

(defun my-remove-alt (x y)
  (cond ((null y) nil)
        ((not (equal x (first y)))
         (cons (first y) (my-remove x (rest y))))
        (t (my-remove x (rest y)))))


;; 8.34. Write MY-INTERSECTION, a recursive version of the
;; INTERSECTION function.

(defun my-intersection (x y)
  (cond ((null x) nil)
        ((member (first x) y)
         (cons (first x)
               (my-intersection (rest x) y)))
        (t (my-intersection (rest x) y))))


;; 8.35. Write MY-SET-DIFFERENCE, a recursive version of the SETDIFFERENCE function.

(defun my-set-difference (x y)
  (cond ((null x) nil)
        ((not (member (first x) y))
              (cons (first x)
                    (my-set-difference (rest x) y)))
        (t (my-set-difference (rest x) y))))


;; 8.36. The function COUNT-ODD counts the number of odd elements in a list
;; of numbers; for example, (COUNT-ODD ’(4 5 6 7 8)) should return
;; two. Show how to write COUNT-ODD using conditional
;; augmentation. Then write another version of COUNT-ODD using the
;; regular augmenting recursion template.
;; (To do this you will need to write a conditional expression for the augmentation value.)

(defun count-odd (x)
  ;; conditional augmentation version
  (cond ((null x) 0)
        ((oddp (first x))
         (+ 1 (count-odd (rest x))))
        (t (count-odd (rest x)))))


(defun count-odd (x)
  ;; regular augmenting version
  (cond ((null x) 0)
        (t (+ (if (oddp (first x))
                  1
                  0)
              (count-odd (rest x))))))


;; Multiple Recursion Example:

(defun fib (n)
  (cond ((equal n 0) 1)
        ((equal n 1) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))

;; 8.37. Define a simple function COMBINE that takes two numbers as input
;; and returns their sum. Now replace the occurence of + in FIB with
;; COMBINE. Trace FIB and COMBINE, and try evaluating (FIB 3) or
;; (FIB 4). What can you say about the relationship between COMBINE,
;; terminal calls, and nonterminal calls?

(defun combine (x y)
  (+ x y))

(defun fib (n)
  (cond ((equal n 0) 1)
        ((equal n 1) 1)
        (t (combine (fib (- n 1)) (fib (- n 2))))))


;; 8.39. Write a function COUNT-ATOMS that returns the number of atoms in
;; a tree. (COUNT-ATOMS ’(A (B) C)) should return five, since in
;; addition to A, B, and C there are two NILs in the tree.

(defun count-atoms (x)
  (cond ((atom x) 1)
        (t (+ (count-atoms (car x))
              (count-atoms (cdr x))))))


;; 8.40. Write COUNT-CONS, a function that returns the number of cons cells
;; in a tree. (COUNT-CONS ’(FOO)) should return one.
;; (COUNTCONS ’(FOO BAR)) should return two.
;; (COUNT-CONS ’((FOO))) should also return two, since the list ((FOO)) requires two cons cells.
;; (COUNT-CONS ’FRED) should return zero.

(defun count-cons (x)
  (cond ((atom x) 0)
        (t (+ 1
              (count-cons (car x))
              (count-cons (cdr x))))))


;; 8.41. Write a function SUM-TREE that returns the sum of all the numbers
;; appearing in a tree.
;; Nonnumbers should be ignored. (SUM-TREE ’((3 BEARS) (3 BOWLS) (1 GIRL))) should return seven.

(defun sum-tree (x)
  (cond ((numberp x) x)
        ((atom x) 0)
        (t (+ (sum-tree (car x))
              (sum-tree (cdr x))))))


;; 8.42. Write MY-SUBST, a recursive version of the SUBST function.

(defun my-subst (new old tree)
  (cond ((equal tree old) new)
        ((atom tree) tree)
        (t (cons (my-subst
                  new old (car tree))
                 (my-subst
                  new old (cdr tree))))))


;; 8.43. Write FLATTEN, a function that returns all the elements of an
;; arbitrarily nested list in a single-level list. (FLATTEN ’((A B (R)) A C
;; (A D ((A (B)) R) A))) should return (A B R A C A D A B R A).

(defun flatten (x)
  (cond ((null x) (list))
    ((atom x) (list x))
	(t (append (flatten (car x))
		   (flatten (cdr x))))))


;; 8.44. Write a function TREE-DEPTH that returns the maximum depth of a
;; binary tree. (TREE-DEPTH ’(A . B)) should return one.
;; (TREEDEPTH ’((A B C D))) should return five, and (TREE-DEPTH ’((A . B) . (C . D))) should return two

(defun tree-depth (x)
  (cond ((atom x) 0)
	(t (+ 1 (max (tree-depth
		      (car x))
		     (tree-depth
		      (cdr x)))))))


;; 8.45. Write a function PAREN-DEPTH that returns the maximum depth of
;; nested parentheses in a list. (PAREN-DEPTH ’(A B C)) should return
;; one, whereas TREE-DEPTH would return three. (PAREN-DEPTH ’(A
;; B ((C) D) E)) should return three, since there is an element C that is
;; nested in three levels of parentheses. Hint: This problem can be solved
;; by CAR/CDR recursion, but the CAR and CDR cases will not be
;; exactly symmetric.

(defun paren-depth (x)
  (cond ((atom x) 0)
	(t (max (+ 1 (paren-depth (first x)))
		(paren-depth (rest x))))))


;; Using Helping Functions:

;; For example, suppose we want to write a function COUNT-UP that counts from one up to n:

;; (count-up 5) => (1 2 3 4 5)

;; This problem is harder than COUNT-DOWN because the innermost
;; recursive call must terminate the recursion when the input reaches five (in the
;; preceding example), not zero. In general, how will the function know when to
;; stop? The easiest way is to supply the original value of N to the recursive
;; function so it can decide when to stop. We must also supply an extra
;; argument: a counter that tells the function how far along it is in the recursion.
;; The job of the helping function is to provide the initial value for the counter.

(defun count-up (n)
  (count-up-recursively 1 n))

(defun count-up-recursively (cnt n)
  (cond ((> cnt n) nil)
	(t (cons cnt
		 (count-up-recursively
		  (+ cnt 1) n)))))

;; 8.46. Another way to solve the problem of counting upward is to to add an
;; element to the end of the list with each recursive call instead of adding
;; elements to the beginning. This approach doesn’t require a helping
;; function. Write this version of COUNT-UP.

(defun count-up-alt (n)
  (cond ((zerop n) nil)
	(t (append (count-up-alt (- n 1))
		   (list n)))))

;; 8.47. Write MAKE-LOAF, a function that returns a loaf of size
;; N. (MAKE-LOAF 4) should return (X X X X). Use IF instead of COND.

(defun make-loaf (x)
  (if (zerop x) nil
      (cons 'x
	    (make-loaf (- x 1)))))

;; 8.48. Write a recursive function BURY that buries an item under n levels of
;; parentheses. (BURY ’FRED 2) should return ((FRED)), while (BURY
;; ’FRED 5) should return (((((FRED))))). Which recursion template did
;; you use?

(defun bury (x n)
  (cond ((zerop n) x)
	(t (list (bury x (- n 1))))))

;; 8.49. Write PAIRINGS, a function that pairs the elements of two lists.
;; (PAIRINGS ’(A B C) ’(1 2 3)) should return ((A 1) (B 2) (C 3)). You
;; may assume that the two lists will be of equal length.

(defun pairings (x y)
  (cond ((null x) nil)
	(t (cons
	    (list (car x) (car y))
	    (pairings (cdr x) (cdr y))))))


;; 8.50. Write SUBLISTS, a function that returns the successive sublists of a
;; list. (SUBLISTS ’(FEE FIE FOE)) should return ((FEE FIE FOE) (FIE
;; FOE) (FOE)).

(defun sublists (x)
  (cond ((null x) nil)
	(t (cons x
		 (sublists (cdr x))))))


;; 8.51. The simplest way to write MY-REVERSE, a recursive version of
;; REVERSE, is with a helping function plus a recursive function of two
;; inputs. Write this version of MY-REVERSE

(defun my-reverse (x)
  (reverse-recursively x nil))

(defun reverse-recursively (x y)
  (cond ((null x) y)
	(t (reverse-recursively
	    (rest x)
	    (cons (car x) y)))))

;; 8.52. Write MY-UNION, a recursive version of UNION.

(defun my-union (x y)
  (append x (union-recursively x y)))

(defun union-recursively (x y)
  (cond ((null y) nil)
	((member (first y) x)
	 (union-recursively x (rest y)))
	(t (cons (first y)
		 (union-recursively
		  x
		  (rest y))))))

;; 8.53. Write LARGEST-EVEN, a recursive function that returns the largest
;; even number in a list of nonnegative integers. (LARGEST-EVEN ’(5 2
;; 4 3)) should return four. (LARGEST-EVEN NIL) should return zero.
;; Use the built-in MAX function, which returns the largest of its inputs.

(defun largest-even (x)
  (cond ((null x) 0)
	((evenp (car x))
	 (max (car x) (largest-even (rest x))))
	(t (largest-even (rest x)))))


;; 8.54. Write a recursive function HUGE that raises a number to its own
;; power. (HUGE 2) should return 2^2
;; , (HUGE 3) should return 3^3 = 27,
;; (HUGE 4) should return 4^4 = 256, and so on. Do not use REDUCE.

(defun huge (x)
  (huge-helper x x))

(defun huge-helper (x n)
  (cond ((equal n 0) 1)
	(t (* x (huge-helper x (- n 1))))))


;; 8.56. Write EVERY-OTHER, a recursive function that returns every other
;; element of a list—the first, third, fifth, and so on. (EVERY-OTHER
;; ’(A B C D E F G)) should return (A C E G). (EVERY-OTHER ’(I
;; CAME I SAW I CONQUERED)) should return (I I I).

(defun every-other (x)
  (cond ((null x) nil)
	(t (cons (car x)
		 (every-other (cdr (cdr x)))))))


(setf family
      '((colin nil nil)
	(deirdre nil nil)
	(arthur nil nil)
	(kate nil nil)
	(frank nil nil)
	(linda nil nil)
	(suzanne colin deirdre)
	(bruce arthur kate)
	(charles arthur kate)
	(david arthur kate)
	(ellen arthur kate)
	(george frank linda)
	(hillary frank linda)
	(andre nil nil)
	(tamara bruce suzanne)
	(vincent bruce suzanne)
	(wanda nil nil)
	(ivan george ellen)
	(julie george ellen)
	(marie george ellen)
	(nigel andre hillary)
	(frederick nil tamara)
	(zelda vincent wanda)
	(joshua ivan wanda)
	(quentin nil nil)
	(robert quentin julie)
	(olivia nigel marie)
	(peter nigel marie)
	(erica nil nil)
	(yvette robert zelda)
	(diane peter erica)))


;; Each person in the database is represented by an entry of form
;; (name father mother)
;; When someone’s father or mother is unknown, a value of NIL is used.
;; The functions you write in this keyboard exercise need not be recursive,
;; except where indicated. For functions that return lists of names, the exact
;; order in which these names appear is unimportant, but there should be no
;; duplicates.

;; 8.60. If the genealogy database is already stored on the computer for you,
;; load the file containing it. If not, you will have to type it in as it
;; appears in Figure 8-12. Store the database in the global variable
;; FAMILY.

;; a. Write the functions FATHER, MOTHER, PARENTS, and
;; CHILDREN that return a person’s father, mother, a list of his or her
;; known parents, and a list of his or her children, respectively.
;; (FATHER ’SUZANNE) should return COLIN. (PARENTS
;; ’SUZANNE) should return (COLIN DEIRDRE). (PARENTS
;; ’FREDERICK) should return (TAMARA), since Frederick’s father
;; is unknown. (CHILDREN ’ARTHUR) should return the set
;; (BRUCE CHARLES DAVID ELLEN). If any of these functions is
;; given NIL as input, it should return NIL. This feature will be useful
;; later when we write some recursive function

(defun father (x)
  (second (assoc x family)))

(defun mother (x)
  (third (assoc x family)))

(defun parents (x)
  (union (and (father x) (list (father x)))
	 (and (mother x) (list (mother x)))))

(defun children (parent)
  (and parent
       (mapcar #'first
	       (remove-if-not
		#'(lambda (entry)
		    (member parent (rest entry)))
		family))))

;; b. Write SIBLINGS, a function that returns a list of a person’s siblings,
;; including genetic half-siblings. (SIBLINGS ’BRUCE) should return
;; (CHARLES DAVID ELLEN). (SIBLINGS ’ZELDA) should return
;; (JOSHUA).

(defun siblings (x)
  (set-difference (union (children (father x))
			 (children (mother x)))
		  (list x)))

;; c. Write MAPUNION, an applicative operator that takes a function and
;; a list as input, applies the function to every element of the list, and
;; computes the union of all the results. An example is (MAPUNION
;; #’REST ’((1 A B C) (2 E C J) (3 F A B C D))), which should return
;; the set (A B C E J F D). Hint: MAPUNION can be defined as a
;; combination of two applicative operators you already know.

(defun mapunion (fn x)
  (reduce #'union (mapcar fn x)))

;; d. Write GRANDPARENTS, a function that returns the set of a
;; person’s grandparents. Use MAPUNION in your solution.

(defun grandparents (x)
  (mapunion #'parents (parents x)))


