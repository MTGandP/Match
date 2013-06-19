;;;;
;;;; patterns.lisp
;;;; -------------
;;;; Created by Michael Dickens on 2013-06-19.
;;;; 
;;;; Default patterns for use with the pattern matching library.
;;;; 

(load "match")

;; Do not print style-warning errors.
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))


(in-package :match)

(match:defpattern list (&rest arglist) 
  (expr)
    (and (listp expr) 
	 (= (length arglist) (length expr)))
    (arglist num (nth num expr)))


;;; Matches a quoted s-expression. Notice that symbols must be treated
;;; with care within patterns: If you write 'x, it will be treated not
;;; as the symbol x, but as the pattern (quote x). To match the symbol
;;; 'x, use the pattern (quote 'x).
;;; Although quote is defined externally, it is special because a
;;; quote inside a pattern does not treat anything inside the quote as
;;; a pattern, so it terminates pattern nesting. This behavior is
;;; useful for matching quoted symbols or lists.
(match:defpattern quote (arg)
  (expr)
    (equal arg expr)
    (arg expr))

;;; defpattern for types
;; TODO: restructure program so that defpattern may bind args or it
;; may not. For example, (cons x xs) binds args x and xs, but 
;; (type 'number) does not bind any args.
(match:defpattern type (type-sym) 
  (expr)
    (typep expr type-sym))

;;; example defpattern for a cons cell
(match:defpattern cons (x xs) 
  (expr)
    (consp expr)
    (x (car expr))
    (xs (cdr expr)))
     
;;; example defpattern for something more complicated and esoteric.
;;; Determines whether a non-empty list contains only even numbers.
(match:defpattern all-even (first &rest arglist)
  (expr)
    (and (listp expr)
	 (= (1+ (length arglist)) (length expr))
	 (>= (length expr) 1)
	 (reduce (lambda (pred x) (and pred (evenp x))) 
		 expr :initial-value t))
    (first (/ (car expr) 2))
    ;; TODO: try dividing each element by 2
    (arglist num (/ (nth num (cdr expr)) 2)))



;; Resume printing style-warning errors.
(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))
