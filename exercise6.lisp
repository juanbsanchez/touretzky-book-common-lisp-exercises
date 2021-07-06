
;;; LIST DATA STRUCTURES
;;; --------------------

(defun add-to-end (x e)
  "Adds element E to the end of list X."
  (append x (list e)))

(add-to-end '(a b c) 'd) ;; => (A B C D)


;; REVERSE => returns the reversal of a list.
;; Like APPEND, REVERSE is nondestructive. It copies its input rather than modifying it.

;; We can use REVERSE to add an element to the end of a list, as follows.

(defun add-to-end (x y)
  (reverse (cons y (reverse x))))

(add-to-end '(a b c) 'd) ;; => (a b c d)

;; NTHCDR => returns the nth successive cdr of a list.

(nthcdr 1 '(a b c)) ;; => (b c)
(nthcdr 3 '(a b c)) ;; => nil
(nthcdr 4 '(a b c . d)) ;; => Error. D is not a list.

;; NTH => takes the CAR of the NTHCDR of a list.

(defun nth (n x)
  "Returns the Nth element of the list x,
  counting from 0."
  (car (nthcdr n x)))

(nth 0 '(a b c)) ;; => a
(nth 1 '(a b c)) ;; => b
(nth 3 '(a b c)) ;; => nil


;; EXERCISES
;; ---------

;; 6.1 Why is (NTH 4 '(A B C)) equal to NIL? => because last position of the list is 2.
;; 6.2 What is the value of (NTH 3 '(A B C . D)), and why? Error, because D isn't a list.


;; LAST returns the last cons cell of a list.

(last '(all is forgiven)) ;; => (forgiven)
(last nil) ;; => nil
(last '(a b c . d)) ;; (c . d)
(last 'nevermore) ;; => Error! NEVERMORE is not a list.

;; EXERCISES
;; ---------

;; 6.3 What is the value of (LAST '(ROSEBUD))? => (ROSEBUD)
;; 6.4 What is the value of (LAST '((A B C))), and why? => ((A B C)) because is a list inside of a list, so returns that list.


;; REMOVE removes an item from a list. The result returned by REMOVE is a new list, without the deleted items.

(remove 'a '(b a n a n a)) ;; => (b n n)
(remove 1 '( 3 1 4 1 5 9)) ;; => (3 4 5 9)

;; REMOVE is a nondestructive function. It does not change any variables or cons cells when removing elements from a list.
;; REMOVE builds its result out of fresh cons cells by copying (parts of) the list.

(setf spell '( a b r a c a d a b r a)) ;; => (A B R A C A D A B R A)
(remove 'a spell) ;; => ( B R C D B R)
(spell)  ;; => (A B R A C A D A B R A)


;; EXERCISES
;; ---------

;; 6.5 Write an expression to set the global variable LINE to the list (ROSES ARE RED).
;; Then write down what each of the following expressions evaluates to:

(setf line '(ROSES ARE RED))

(reverse line) ;; => (RED ARE ROSES)
(first (last line)) ;; => RED
(nth 1 line) ;; => ARE
(reverse (reverse line)) ;; => ROSES ARE RED
(append line (list (first line))) ;; => (ROSES ARE RED ROSES)
(append (last line) line) ;; => (RED ROSES ARE RED)
(list (first line) (last line)) ;; => (ROSES (RED))
(cons (last line) line) ;; => ((RED) ROSES ARE RED)
(remove 'are line) ;; => (ROSES RED)
(append line '(violets are blue)) ;; => (ROSES ARE RED VIOLETS ARE BLUE)

;; 6.6 Use the LAST function to write a function called LAST-ELEMENT that returns the last element of a list instead of the last cons cell.
;; Write another version of LAST-ELEMENT using REVERSE instead of LAST.
;; Write another version using NTH and LENGTH.

(defun last-element (list)
  (first (last list)))

(defun last-element (list)
  (first (reverse list)))

(defun last-element (list)
  (and list ; to handle NIL correctly
       (nth (- (length list) 1) list)))


;; 6.7 Use REVERSE to write a NEXT-TO-LAST function that returns the
;; next-to-last element of a list. Write another version using NTH.

(defun next-to-last (x)
  (and x
       (nth (- (length x) 2) x)))

(defun last-element (x)
  (second (reverse x)))

;; 6.8 Write a function MY-BUTLAST that returns a list with the last element
;; removed. (MY-BUTLAST ’(ROSES ARE RED)) should return the list
;; (ROSES ARE). (MY-BUTLAST ’(G A G A)) should return (G A G).

(defun my-butlast (x)
  (reverse (cdr (reverse x))))


;; 6.9 What primitive function does the following reduce to?

(defun mystery (x)
  (first (last (reverse x)))) ;; => returns first element of a list


;; 6.10 A palindrome is a sequence that reads the same forwards and
;; backwards. The list (A B C D C B A) is a palindrome; (A B C A B C)
;; is not. Write a function PALINDROMEP that returns T if its input is a palindrome.

(defun palindromep (x)
  (equal x (reverse x)))

;; 6.11 Write a function MAKE-PALINDROME that makes a palindrome out
;; of a list, for example, given (YOU AND ME) as input it should return (YOU AND ME ME AND YOU).

(defun make-palindrome (x)
  (append x (reverse x)))


;; INTERSECTION => takes the intersection of two sets and returns a list of items appearing in both sets.
;; The exac order in which elements appear in the result is undefined;

(intersection '(fred john mary)
              '(sue mary fred))

;; (FRED MARY)

;; If a list contains multiple ocurrence of an item, it is not a true set.

;; 6.15  We can use MEMBER to write a predicate that returns a true value if a sentence contains the word ‘‘the.’’

(defun contains-the-p (sent)
  (member ’the sent))


;; Suppose we instead want a predicate CONTAINS-ARTICLE-P that
;; returns a true value if a sentence contains any article, such as ‘‘the,’’
;; ‘‘a,’’ or ‘‘an.’’ Write a version of this predicate using
;; INTERSECTION. Write another version using MEMBER and OR.

(defun contains-article-p (sent)
  (intersection sent
                '(the a an)))

(defun contains-article-p (sent)
  (or (member 'the sent)
      (member 'a sent)
      (member 'an sent)))


;; 6.18 Write a function ADD-VOWELS that takes a set of letters as input and adds the vowels (A E I O U) to the set.
;; For example, calling ADDVOWELS on the set (X A E Z) should produce the set (X A E Z I O U), except that the exact order of the elements in the result is unimportant.

(defun add-vowels (set)
  (union set
         '(a e i o u)))


;; UNION => returns the union of two sets, in other words, a list of items that appear in either set.

(union '(finger hand arm)
       '(toe finger foot leg)) ;; => (FINGER HAND ARM TOE FOOT LEG)

;; SET-DIFFERENCE => performs set subtraction. Returns what is left of first set when elements in the second set have been removed.

(set-difference (alpha bravo charlie delta)
                '(bravo charlie)) ;; => (ALPHA DELTA)

(set-difference '(alpha bravo charlie delta)
                '(echo alpha foxtrot)) ;; => (BRAVO CHARLIE DELTA)

;; SUBSETP predicate returns T if one set is contained in another, in other words, if every element of the first set is an element of the second set.

(subsetp '(a i) '(a e i o u)) ;; => t
(subsetp '(a x) '(a e i o u)) ;; => nil


;; 6.21 If set x is a subset of set y, then subtracting y from x should leave the
;; empty set. Write MY-SUBSETP, a version of the SUBSETP predicate
;; that returns T if its first input is a subset of its second input.

(defun my-subsetp (x y)
  (null (set-difference x y)))

;; function null returns t if object is the empty list, otherwise returns nil.


;; Here is an example of how to solve a modest programming problem using
;; sets. The problem is to write a function that adds a title to a name, turning
;; ‘‘John Doe’’ into ‘‘Mr. John Doe’’ or ‘‘Jane Doe’’ into ‘‘Ms. Jane Doe.’’ If
;; a name already has a title, that title should be kept, but if it doesn’t have one,
;; we will try to determine the gender of the first name so that the appropriate
;; title can be assigned.
;; To solve a problem like this, we must break it down into smaller pieces.
;; Let’s start with the question of whether a name has a title or not. Here’s how
;; we’d write a function to answer that question:

(defun titledp (name)
  (member (first name) '(mr ms miss mrs)))

(titledp '(jane doe)) ;; NIL
(titledp '(ms jane doe)) ;; (MS MISS MRS)


;; The next step is to write functions to figure out whether a word is a male or
;; female first name. We will use only a few instances of each type of name to
;; keep the example brief.

(setf male-first-names
      '(john kim richard fred george))

(setf female-first-names
      '(jane mary wanda barbara kim))

(defun malep (name)
  (and (member name male-first-names)
       (not (member name female-first-names))))

(defun femalep (name)
  (and (member name female-first-names)
       (not (member name male-first-names))))

;; Now we can write the GIVE-TITLE function that adds a title to a name.
;; Of course, we will only add a title if the name doesn’t already have one. If the
;; first name isn’t recognized as male or female, we’ll play it safe and use "Mr. or Ms."

(defun give-title (name)
  "Returns a name with an appropriate title on the front."
  (cond ((titledp name) name)
        ((malep (first name)) (cons 'mr name))
        ((femalep (first name)) (cons 'ms name))
        (t (append '(mr or ms) name))))

(give-title '(miss jane adams)) ;; => (MISS JANE ADAMS)
(give-title '(john q public)) ;; => (MR JOHN Q PUBLIC)
(give-title '(barbara smith)) ;; => (MS BARBARA SMITH)
(give-title '(kim johnson)) ;; => (MR OR MS KIM JOHNSON)

;; Here are a few more things we can do with these lists of names. The
;; functions below take no inputs, so their argument list is NIL.

(defun gender-ambiguous-names ()
  (intersection male-names female-names))

(gender-ambiguos-names) ;; => (kim)

(defun uniquely-male-names ()
  (set-difference male-names female-names))

(uniquely-male-names) ;; => (john richard fred george)


;; MINI KEYBOARD EXERCISE

;; 6.26 We are going to write a program that compares the descriptions of two
;; objects and tells how many features they have in common. The
;; descriptions will be represented as a list of features, with the symbol
;; -VS- separating the first object from the second. Thus, when given a
;; list like

;; (large red shiny cube -vs-
;;        small shiny red four-sided pyramid)

;; the program will respond with (2 COMMON FEATURES). We will
;; compose this program from several small functions that you will write
;; and test one at a time.

;; a. Write a function RIGHT-SIDE that returns all the features to the
;; right of the -VS- symbol. RIGHT-SIDE of the list shown above
;; should return (SMALL SHINY RED FOUR-SIDED PYRAMID).
;; Hint: remember that the MEMBER function returns the entire
;; sublist starting with the item for which you are searching. Test your
;; function to make sure it works correctly

(defun right-side (x)
  (rest (member '-vs- x)))


;; b. Write a function LEFT-SIDE that returns all the features to the left
;; of the -VS-. You can’t use the MEMBER trick directly for this one,
;; but you can use it if you do something to the list first

(defun left-side (x)
  (right-side (reverse x)))

;; c. Write a function COUNT-COMMON that returns the number of
;; features the left and right sides of the input have in common

(defun count-common (x)
  (length (intersection
           (right-side x)
           (left-side x))))


;; d. Write the main function, COMPARE, that takes a list of features
;; describing two objects, with a -VS- between them, and reports the
;; number of features they have in common. COMPARE should return
;; a list of form (n COMMON FEATURES).

(defun compare (x)
  (list (count-common x) 'common 'features))

(compare '(small red metal cube -vs-
           red plastic small cube)) ; => (3 COMMON FEATURES)



;; A table, or association list, is a list of lists.
;; Each list is called an entry, and the car of each entry is its key.

(setf words
      '((one un)
        (two deux)
        (three trois)
        (four quatr)
        (five cinq)))

;; The ASSOC function loos up an entry in a table, given its key.

(assoc 'three words) ;; => (three trois)
(assoc 'six words) ;; => nil


;; RASSOC is like ASSOC, except it looks at the cdr of each element of the table instead of the car.
;; To use RASSOC with symbols as keys, the table must be a list of dotted pairs like this:

(setf sounds
      '((cow . moo)
        (pig . oink)
        (cat . meow)
        (dog . woof)
        (bird . tweet)))

(rassoc 'woof sounds) ;; => (dog . woof)
(assoc 'dog sounds) ;; => (dog. woof)


;; SET-EXCLUSIVE-OR is a built-in Common Lisp function that returns properties appear in the description of the first but not the second,
;; or the description of the second but not the first.

(set-exclusive-or '(small red dull metal cube) '(red small dull plastic cube)) ;; => (plastic metal)


;; 6.30 Make a table called BOOKS of five books and their authors. The first
;; entry might be (WAR-AND-PEACE LEO-TOLSTOY).

(setf books
      '((war-and-peace leo-tolstoy)
        (roma-victoriosa javier-negrete)
        (roma-invicta javier-negrete)
        (spqr-una-historia-de-la-roma-antigua mary-beard)
        (roma-eterna marcos-lopez-herrador)))

;; 6.31 Write the function WHO-WROTE that takes the name of a book as
;; input and returns the book’s author.

(defun who-wrote (x)
  (second (assoc x books)))


;; 6.34 Here is a table of states and some of their cities, stored in the global
;; variable ATLAS:

(setf atlas
      '((pennsylvania pittsburgh)
       (new-jersey newark)
       (pennsylvania johnstown)
       (ohio columbus)
       (new-jersey princeton)
        (new-jersey trenton)))

;; Suppose we wanted to find all the cities a given state contains. ASSOC
;; returns only the first entry with a matching key, not all such entries, so
;; for this table ASSOC cannot solve our problem. Redesign the table so
;; that ASSOC can be used successfully.

(setf redesigned-atlas
      '((pennsylvania (pittsburgh johnstown))
        (new-jersey (newark princeton trenton))
        (ohion (columnbus))))



;; 6.35 In this problem we will simulate the behavior of a very simple-minded
;; creature, Nerdus Americanis (also known as Computerus Hackerus).
;; This creature has only five states: Sleeping, Eating, Waiting-for-aComputer, Programming, and Debugging.
;; Its behavior is cyclic: After
;; it sleeps it always eats, after it eats it always waits for a computer, and
;; so on, until after debugging it goes back to sleep for a while

;; a. What type of data structure would be useful for representing the
;; connection between a state and its successor? Write such a data
;; structure for the five-state cycle given above, and store it in a global
;; variable called NERD-STATES.

(setf nerd-states
      '((sleeping . eating)
        (eating . waiting)
        (waiting . programming)
        (programming . debugging)
        (debugging . sleeping)))


;; b. Write a function NERDUS that takes the name of a state as input
;; and uses the data structure you designed to determine the next state
;; the creature will be in. (NERDUS ’SLEEPING) should return
;; EATING, for example. (NERDUS ’DEBUGGING) should return
;; SLEEPING.

(defun nerdus (state)
  (cdr (assoc state nerd-states)))

;; c. What is the result of (NERDUS ’PLAYING-GUITAR)? => NIL

;; d. When Nerdus Americanis ingests too many stimulants (caffeine overdose), it stops sleeping.
;; After finishing Debugging, it immediately goes on to state Eating.
;; Write a function SLEEPLESSNERD that works just like NERDUS except it never sleeps.
;; Your function should refer to the global variable NERD-STATES, as NERDUS does.

(defun sleepless-nerd (state)
  (let ((x (nerdus state)))
    (if (equal y 'sleeping)
        (nerdus x)
        x)))


;; e. Exposing Nerdus Americanis to extreme amounts of chemical
;; stimulants produces pathological behavior. Instead of an orderly
;; advance to its next state, the creature advances two states. For
;; example, it goes from Eating directly to Programming, and from
;; there to Sleeping. Write a function NERD-ON-CAFFEINE that
;; exhibits this unusual pathology. Your function should use the same
;; table as NERDUS.

(defun nerd-on-caffeine (state)
  (nerdus (nerdus state)))


;; 6.36 Write a function to swap the first and last elements of any list. (SWAPFIRST-LAST ’(YOU CANT BUY LOVE)) should return (LOVE  CANT BUY YOU).

(defun swapfirst-last (x)
  (let* ((a (reverse (rest x)))
          (b (reverse (rest a))))
    (cons (first a)
          (append b (list (first x))))))


;; 6.37 ROTATE-LEFT and ROTATE-RIGHT are functions that rotate the
;; elements of a list. (ROTATE-LEFT ’(A B C D E)) returns (B C D E A),
;; whereas ROTATE-RIGHT returns (E A B C D). Write these functions.


(defun rotate-left (x)
  (reverse (cons (first x)
                 (reverse (cdr x)))))

(defun rotate-left (x)
  (append (rest x) (list (first x))))

(defun rotate-right (x)
  (append
   (list (first (last x)))
   (reverse (rest (reverse x)))))

(defun rotate-right (x)
  (let ((r (reverse x)))
    (cons (first r)
          (reverse (rest r)))))


;; In this keyboard exercise we will write some routines for moving Robbie the
;; robot around in a house. The map of the house appears in Figure 6-2. Robbie
;; can move in any of four directions: north, south, east, or west.

;; The layout of the house is described in a table called ROOMS, with one
;; element for each room:

(setf rooms
      '((living-room (north front-stairs)
                     (south dining-room)
                     (east kitchen))
        (upstairs-bedroom (west library)
                          (south front-stairs))
        (dining-room (north living-room)
                     (east pantry)
                     (west downstairs-bedroom))
        (kitchen (west living-room)
                 (south pantry))
        (pantry (north kitchen)
                (west dining-room))
        (downstairs-bedroom (north back-stairs)
                            (east dining-room))
        (back-stairs (south downstairs-bedroom)
                     (north library))
        (front-stairs (north upstairs-bedroom)
                      (south living-room))
        (library (east upstairs-bedroom)
                 (south back-stairs))))

;; 6.41 If the table of rooms is already stored on the computer for you, load the
;; file containing it. If not, you will have to type the table in as it appears
;; in Figure 6-3. If you like, try (SDRAW ROOMS) or (SCRAWL ROOMS) to view the table as a cons cell structure.

;; a. Write a function CHOICES that takes the name of a room as input
;; and returns the table of permissible directions Robbie may take from
;; that room. For example, (CHOICES ’PANTRY) should return the
;; list ((NORTH KITCHEN) (WEST DINING-ROOM)). Test your
;; function to make sure it returns the correct result.

(defun choices (room)
  (cdr (assoc room rooms)))


;; b. Write a function LOOK that takes two inputs, a direction and a
;; room, and tells where Robbie would end up if he moved in that
;; direction from that room. For example, (LOOK ’NORTH ’PANTRY) should return KITCHEN. (LOOK ’WEST ’PANTRY)
;; should return DINING-ROOM. (LOOK ’SOUTH ’PANTRY)
;; should return NIL. Hint: The CHOICES function will be a useful
;; building block

(defun look (direction room)
  (second (assoc direction (choices room))))

;; c. We will use the global variable LOC to hold Robbie’s location.
;; Type in an expression to set his location to be the pantry. The
;; following function should be used whenever you want to change his
;; location.

(setf loc 'pantry)

(defun set-robbie-location (place)
  "Moves Robbie to PLACE by setting
the variable LOC."
  (setf loc place))


;; d. Write a function HOW-MANY-CHOICES that tells how many
;; choices Robbie has for where to move to next. Your function should
;; refer to the global variable LOC to find his current location. If he is
;; in the pantry, (HOW-MANY-CHOICES) should return 2.

(defun how-many-choices ()
  (length (choices loc)))

