;;;;
;;;; patterns.lisp
;;;; -------------
;;;; Created by Michael Dickens on 2013-06-19.
;;;; 
;;;; Default patterns for use with the pattern matching library.
;;;; 

;; Do not print style-warning errors.
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))


(match:defpattern list (&rest arglist) 
  (expr)
  "Match a list with as many arguments as ARGLIST has.

Example
  (matchp '(1 2 3) (list x y z)) ; t
  (matchp '(1 2 3) (list x y)) ; nil
  (matchp '(1 2 (3 4)) (list w x (list y z)) ; t
  (matchp nil (list) ; t
"
    (and (listp expr) 
	 (= (length arglist) (length expr)))
    (arglist num (nth num expr)))

(match:defpattern quote (arg)
  (expr)
  "Match a quoted s-expression. Remember that Lisp automatically
  expands 'x into (quote x), so you may write 'x in a form and it will
  expand into the pattern (quote x).

Although QUOTE is defined externally, it is special because a quote
  inside a pattern does not treat anything inside the quote as a
  pattern, so it terminates pattern nesting. This behavior is useful
  for matching quoted symbols or lists.

Example
  (match '(+ x 3) 
    ('x 'no)
    ((+ x 3) 'no)
    ('(+ x 3) 'yes))
"
    (equal arg expr)
    (arg expr))

(match:defpattern type (type-sym) 
  (expr)
  "Match the type of an expression.

type-sym: A quoted symbol giving the type to match against.

Example
  (matchp 3 (type 'number)) ; t
  (matchp 3 (type 'string)) ; nil
"
    (and (consp type-sym)
	 (equal (car type-sym) 'quote)
	 (typep expr (cadr type-sym))))

(match:defpattern cons (x xs) 
  (expr)
  "Match a cons cell.

x: The car of the cons cell.
xs: The cdr of the cons cell.

Example
  (matchp '(1 2) (cons 1 (list 2)) ; t
  (matchp '(1) (cons 1 nil)) ; t
"
    (consp expr)
    (x (car expr))
    (xs (cdr expr)))
     
;;; used to bind a variable when using a form that does not have binding
(match:defpattern bind (var form)
  (expr)
  "This pattern is used in conjunction with patterns that do not bind
variables. If you wish to match an expression against a pattern that
does not bind variables and you want to bind the expression to a
variable, you may use this pattern.

var: The name of the variable to which to bind the expression's value.
form: The form used to match the expression. This form must be quoted,
or else it will incorrectly evaluate as a nested pattern.

Example
  Suppose you want to match an expression's type and use it as a
  variable, but (type) does not bind variables. Then you can use the
  (bind) form, like so: 
    (match expr ((bind x (type 'number)) (do-something)))
  This matches expr against (type 'number). If it matches, it binds
  expr to x.
"
    ;; form is actually a quoted form, so remove the quote
    (quoted-matchp expr (cadr form))
    (var expr))

;; Resume printing style-warning errors.
(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))
