;;;;
;;;; test.lisp
;;;; ---------
;;;; Created by Michael Dickens on 2013-06-19.
;;;; 
;;;; Tests for the pattern matching library.
;;;; 

;; Do not print style-warning errors.
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))


(load "patterns")

;; TODO: put unit.lisp in same directory
(load "../unit")

(import 'match)
(import 'unit)

;;; Functions demonstrating pattern matching.
(defmatch factorial
  (0 1)
  (n (* n (factorial (1- n)))))

(defmatch tail-recursive-factorial 
  ;; If called with just 1 argument, add the other argument
  (n (tail-recursive-factorial n 1))
  ;; Do the actual computation
  (0 total total)
  (n total (tail-recursive-factorial (1- n) (* total n))))

(defmatch ackermann
  (0 n (1+ n))
  (m 0 (ackermann (1- m) 1))
  (m n (ackermann (1- m) (ackermann m (1- n)))))

(defmatch drop
  (n nil nil)
  (0 xs xs)
  (n (cons x xs) (drop (1- n) xs)))

(defmatch take
  (n nil nil)
  (0 xs nil)
  (n (cons x xs) (cons x (take (1- n) xs))))

(defun unit-test ()
  (check 
    verbose-off
    
    "Test proper compile-time error handling"
    (equal
     (handler-case 
	 (eval '(defpattern something (x) (wrong number special args) ()))
       (defpattern-wrong-number-special-args (err) "error found"))
     "error found")

    "Test basic expressions using primitives"
    (equal (match 3 (3 'yes))
	   'yes)
    (equal (match 3 (5 'yes) (t 'no))
	   'no)
    (equal (match "hello" ("HEllo" 'wrong) ("hello" 'right))
	   'right)

    "Test basic expressions using symbols and variables"
    (equal (let ((expr '('this 'has 4 'words))) (match expr (t expr)))
    	   '('this 'has 4 'words))
    (equal (let ((expr '(+ x 3))) 
	     (match expr (expr 'yes)))
	   'yes)

    ;; TODO: We should be able to use (type 'number).
    "Test user-defined type pattern"
    (equal (match 3 ((type number) 'yes) (t 'no))
	   'yes)
    (equal (match "hello" ((type number) 'yes) (t 'no))
	   'no)
    (equal (match 'world ((type number) 'num) ((type symbol) 'sym) (t 'no))
	   'sym)

    ;; "Test user-defined quote pattern"
    ;; ;; TODO: Get these to work. QUOTE is causing problems because it
    ;; ;; thinks QUOTE is a pattern, not an actual quote.
    (equal (let ((expr 'thing)) (match expr ('thing 'yes)))
	   'yes)
    (equal (match 'world ("world" 'wrong-str) 
		  ('other 'wrong-sym)
    		  ('world 'right))
    	   'right)
    (equal (match '(+ x 3) 
	     ('x 'wrong-structure) 
	     ('(+ y 3) 'wrong-symbol-name)
	     ('(+ x 4) 'wrong-number)
	     ('(+ x 3) 'right))
    	   'right)

    "Test user-defined cons pattern"
    (equal (match (list 1 2) ((cons x y) (list x y)))
	   '(1 (2)))
    (equal (match (list 1) ((cons x y) (list x y)))
	   (list 1 nil))

    "Test user-defined list pattern"
    (equal (match (list 1) ((list x) x))
	   1)
    (equal (match (list 1 2 3) ((list x y z) (+ x y z)))
	   6)
    (equal (match nil ((list) t) (t nil))
	   t)

    "Test user-defined all-even pattern"
    (equal (match (list 2 4 6) ((all-even x y z) (list x y z)))
	   (list 1 2 3))
    (equal (match (list 28) ((all-even x) (list x)))
	   (list 14))
    (equal (match (list 28) ((all-even x y) (list x y)))
	   nil)
    (equal (match (list 5) ((all-even x) (list x)))
	   nil)
    (equal
     (handler-case
	 (match (list 5 8) ((all-even) "accidentally succeeded"))
       (sb-int:simple-program-error (err) "error found"))
     "error found")

    "Test nested patterns"
    (equal (match (list 1 2) ((cons x (cons y z)) (list x y z)))
	   (list 1 2 nil))
    (equal (match (cons "not" "cons") 
		  ((cons x (cons y z)) (list x y z))
		  ((cons "cons" y) 'wrong)
		  ((cons "not" y) (concatenate 'string y " cell")))
	   "cons cell")
    (equal (match (list (list 1 2) 3) 
		  ((list (list x y) z) (list x y z)))
	   (list 1 2 3))
    (equal (match (list 4 5 6)
		  ((cons x (list y z)) (+ x y z)))
	   (+ 4 5 6))
    (equal (match (list 7 8)
		  ((cons x (cons 7 y)) 'wrong)
		  ((cons x (cons 8 y)) (list x y)))
	   (list 7 nil))

    "Test factorial function"
    (equal (factorial 0)
	   1)
    (equal (factorial 1)
	   1)
    (equal (factorial 2)
	   2)
    (equal (factorial 6)
	   720)

    "Test tail-recursive factorial function"
    (equal (tail-recursive-factorial 0)
	   1)
    (equal (tail-recursive-factorial 1)
	   1)
    (equal (tail-recursive-factorial 2)
	   2)
    (equal (tail-recursive-factorial 6)
	   720)

    "Test Ackermann function"
    (equal (ackermann 0 0) 
	   1)
    (equal (ackermann 1 0)
	   2)
    (equal (ackermann 2 0)
	   3)
    (equal (ackermann 4 0)
	   13)
    (equal (ackermann 0 1)
	   2)
    (equal (ackermann 0 2)
	   3)
    (equal (ackermann 1 1)
	   3)
    (equal (ackermann 2 2)
	   7)
    (equal (ackermann 3 3)
	   61)

    "Test drop function"
    (equal (drop 0 nil)
	   nil)
    (equal (drop 0 '(1 2 3))
	   '(1 2 3))
    (equal (drop 4 nil)
	   nil)
    (equal (drop 1 '(1 2 3))
	   '(2 3))
    (equal (drop 2 '(1 2 3))
	   '(3))
    (equal (drop 3 '(1 2 3))
	   nil)
    (equal (drop 4 '(1 2 3))
	   nil)

    "Test take function"
    (equal (take 0 nil)
	   nil)
    (equal (take 0 '(1 2 3))
	   nil)
    (equal (take 1 '(1 2 3))
	   '(1))
    (equal (take 2 '(1 2 3))
	   '(1 2))
    (equal (take 3 '(1 2 3))
	   '(1 2 3))
    (equal (take 4 '(1 2 3))
	   '(1 2 3))
    ))

(defun unit-test-safe ()
  "Calls (unit-test) and catches exceptions."
  (handler-case (unit-test) 
    (match-error (se)
      (format t "~a: ~a" se (slot-value se 'text)))))

(defun benchmark ()
  "Used to benchmark (match). Run this only after all the unit tests
  pass."
  (ackermann 3 7)
  (ackermann 3 9))



;; Resume printing style-warning errors.
(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))
