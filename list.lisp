;;;;
;;;; list.lisp
;;;; ---------
;;;; Created by Michael Dickens on 2013-06-19.
;;;; 
;;;; Uses match to implement some basic list functions.
;;;; 

(load "match")
(load "unit")

(in-package :match)


;; Do not print style-warning errors.
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))


(defmatch m-head
  (nil nil)
  ((cons x xs) x))

(defmatch m-tail
  (nil nil)
  ((cons x xs) xs))

(defmatch m-length
  (nil 0)
  ((cons x xs) (1+ (m-length xs))))

;; TODO: This seems overly complicated. There must be a way to
;; simplify it. 
(defmatch m-index
  (val nil nil)
  (val (cons x xs)
    (if (equal x val)
	0
	(let ((inner (m-index val xs)))
	  (if inner (1+ inner) nil)))))

(defmatch m-drop
  (n nil nil)
  (0 xs xs)
  (n (cons x xs) (m-drop (1- n) xs)))

(defmatch m-take
  (n nil nil)
  (0 xs nil)
  (n (cons x xs) (cons x (m-take (1- n) xs))))

;; TODO: It should be possible to do something like 
;; (type 'function f)
(defmatch m-map
  (f nil nil)
  (f (cons x xs) (cons (funcall f x) (m-map f xs))))


(defun test-list ()
  (unit:check
    unit:verbose-off

    "m-head"
    (equal (m-head nil)
	   nil)
    (equal (m-head '(1))
	   1)
    (equal (m-head '(2 1))
	   2)
    (equal (m-head '((2) 1))
	   '(2))

    "m-tail"
    (equal (m-tail nil)
	   nil)
    (equal (m-tail '(1))
	   nil)
    (equal (m-tail '(1 2))
	   '(2))
    (equal (m-tail '((1) 2 3))
	   '(2 3))

    "m-length"
    (equal (m-length nil)
	   0)
    (equal (m-length '(1))
	   1)
    (equal (m-length '(nil nil nil nil nil))
	   5)

    (equal (m-index 1 '(1 2 3))
	   0)
    (equal (m-index 2 '(1 2 3))
	   1)
    (equal (m-index 3 '(1 2 3))
	   2)
    (equal (m-index 4 '(1 2 3))
	   nil)

    "m-drop"
    (equal (m-drop 0 nil)
	   nil)
    (equal (m-drop 0 '(1 2 3))
	   '(1 2 3))
    (equal (m-drop 4 nil)
	   nil)
    (equal (m-drop 1 '(1 2 3))
	   '(2 3))
    (equal (m-drop 2 '(1 2 3))
	   '(3))
    (equal (m-drop 3 '(1 2 3))
	   nil)
    (equal (m-drop 4 '(1 2 3))
	   nil)

    "m-take"
    (equal (m-take 0 nil)
	   nil)
    (equal (m-take 0 '(1 2 3))
	   nil)
    (equal (m-take 1 '(1 2 3))
	   '(1))
    (equal (m-take 2 '(1 2 3))
	   '(1 2))
    (equal (m-take 3 '(1 2 3))
	   '(1 2 3))
    (equal (m-take 4 '(1 2 3))
	   '(1 2 3))

    "m-map"
    (equal (m-map (lambda (x) x) nil)
	   nil)
    (equal (m-map (lambda (x) x) '(7 8))
	   '(7 8))
    (equal (m-map (lambda (x) (* x 2)) '(1 2 3 4))
	   '(2 4 6 8))

    ))



;; Resume printing style-warning errors.
(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))