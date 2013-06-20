;;;;
;;;; patterns.lisp
;;;; -------------
;;;; Created by Michael Dickens on 2013-06-19.
;;;; 
;;;; Default patterns for use with the pattern matching library.
;;;; 

;; Do not print style-warning errors.
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))


;; TODO: Add better documentation for these patterns.



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
    (and (consp type-sym)
	 (equal (car type-sym) 'quote)
	 (typep expr (cadr type-sym))))

;;; example defpattern for a cons cell
(match:defpattern cons (x xs) 
  (expr)
    (consp expr)
    (x (car expr))
    (xs (cdr expr)))
     
;;; used to bind a variable when using a form that does not have binding
(match:defpattern bind (var pred)
  (expr)
    ;; pred is a quoted predicate, so remove the quote
    (matchp expr (cadr pred))
    (var expr))

;; Resume printing style-warning errors.
(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))
