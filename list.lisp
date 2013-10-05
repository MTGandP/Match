;;;;
;;;; list.lisp
;;;; ---------
;;;; Created by Michael Dickens on 2013-06-19.
;;;; 
;;;; Uses match to implement some basic list functions. The functions
;;;; in this file are written simply and documented to help explain
;;;; the Match library by example.
;;;; 

(load "match")
(load "unit")


;; Do not print style-warning errors.
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))


(match:defmatch m-head
  "Perform the same function as #'car."
  ;; Given nil, return nil.
  (nil nil)

  ;; Given a cons cell, return the car of the cell.
  ((cons x xs) x))

(match:defmatch m-tail
  "Perform the same function as #'cdr."
  ;; Given nil, return nil.
  (nil nil)

  ;; Given a cons cell, return the cdr of the cell.
  ((cons x xs) xs))

(match:defmatch m-drop
  "Drop the last n elements of the list and returns a list containing
  only the remaining elements."
  ;; Given any number and an empty list, return nil.
  (n nil nil)

  ;; Given 0 and any list, return the list.
  (0 xs xs)

  ;; Given a number and a cons cell, recurse on the cdr of the cell.
  (n (cons x xs) (m-drop (1- n) xs)))

(match:defmatch m-take
  "Return a list containing only the first n elements of the input 
list."
  ;; Given any number and an empty list, return nil.
  (n nil nil)

  ;; Given 0 and any list, return nil.
  (0 xs nil)

  ;; Given a number and a cons cell, recurse on the cdr of the cell.
  (n (cons x xs) (cons x (m-take (1- n) xs))))

(match:defmatch m-length
  "Find the length of a list."
  ;; nil has length 0.
  (nil 0)

  ;; For a cons cell, define its length recursively in terms of its cdr.
  ((cons x xs) (1+ (m-length xs))))

;; TODO: This seems unnecessarily complicated. There must be a way to
;; simplify it.
(match:defmatch m-index
  "Where the first argument is a value and the second is a list, find
the index of the first argument in the second argument. If it is not
found, return nil."
  ;; An empty list contains nothing.
  (val nil nil)

  (val (cons x xs)
    (if (equal x val)
        ;; If val is found at the beginning of this list, its index is 0.
        0

      ;; Otherwise, find the index of val in the cdr and add 1.
      (let ((inner (m-index val xs)))
        (if inner (1+ inner) nil)))))

(match:defmatch m-map
  "Given a function and a list, map over the list and apply the
  function to each element of the list."
  ;; Given a function f and nil, simply return nil.
  ((bind f '(type 'function)) nil 
   nil)

  ;; Given a function and a cons cell, apply the function to the car
  ;; and recursively call m-map on the cdr.
  ((bind f '(type 'function)) (cons x xs) 
   (cons (funcall f x) (m-map f xs))))


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
