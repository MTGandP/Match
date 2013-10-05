;;;;
;;;; match.lisp
;;;; ----------
;;;; Created by Michael Dickens on 2013-06-06.
;;;; 
;;;; Simple pattern matching for Common Lisp.
;;;; 

;; Do not print style-warning errors.
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))


(defpackage :match
  (:use :common-lisp)
  (:export :defmatch
	   :defpattern
	   :match
	   :matchp
	   :patternp
	   :quoted-matchp))

(in-package :match)


(defmacro define-match-error (name)
  "A basic macro for quickly defining new match errors."
  `(define-condition ,name (match-error)
     ((text :initarg :text :reader text))))

(define-condition match-error (error)
  ((text :initarg :text :reader text)))

(define-condition defpattern-error (match-error)
  ((text :initarg :text :reader text)))

(define-condition defpattern-args-mismatch (defpattern-error)
  ((text :initarg :text :reader text)))

(define-condition defpattern-wrong-number-special-args (defpattern-error)
  ((text :initarg :text :reader text)))

(define-condition defpattern-unknown-argument (defpattern-error)
  ((text :initarg :text :reader text)))

(define-condition malformed-match-args (match-error)
  ((text :initarg :text :reader text)))

(define-condition pattern-wrong-argnum (match-error)
  ((text :initarg :text :reader text)))

(define-condition undefined-pattern (match-error)
  ((text :initarg :text :reader text)))

(defun booleanp (x)
  (or (eq x t) (eq x nil)))

(defclass Pattern ()
  ((num-args :initarg :num-args)
   (doc-string :initarg :doc-string)
   (matchp-test :initarg :matchp-test)
   (value-getter :initarg :value-getter)))

(let ((patterns (make-hash-table)))
  (defun patternp (name)
    "Determine if the given symbol defines a pattern."
    (gethash name patterns))

  (defun get-pattern (name)
    "Return the pattern with the given name."
    (gethash name patterns))

  (defun set-pattern (name value)
    "Set the pattern with the given name to the given value. If the
    name is not bound, create a new pattern with the given name and
    bind it to the given value."
    (setf (gethash name patterns) value)))

(defun basic-matchp (expr form)
  "Determine if the expression matches the form. Only uses certain
basic built-in patterns and NOT user-defined patterns."
  (cond
    ;; T matches everything.
    ((equal form t) t)

    ;; nil only matches nil.
    ((null form) (null expr))

    ;; A symbol matches everything.
    ((symbolp form) t)

    ;; A number must match the same number using =.
    ((and (numberp expr) (numberp form)) (= expr form))

    ;; Everything else is matched using equal.
    (t (equal expr form))))

(defun quoted-matchp (expr form)
  "Determine whether the expression matches the form. The form must
be quoted.

Example
  (quoted-matchp 3 '(type 'number))"
  (cond
    ((and (consp form) (symbolp (car form)))
     (let ((pattern (get-pattern (car form))))
       (and 
	;; Match the outermost form
	(if pattern
	    (apply (slot-value pattern 'matchp-test) expr (cdr form))
	    (error 'undefined-pattern :text
		   (format nil "Undefined pattern ~a." (car form))))
	;; Recursively match inner forms but not if the form is QUOTE
	(if (equal (car form) 'quote)
	  t
	  (notany #'null
	    (loop for i from 0 below (length (cdr form)) collect
		 (handler-case
		     (quoted-matchp 
		      (funcall (slot-value pattern 'value-getter)
			       expr i)
		      (nth i (cdr form)))
		   (pattern-wrong-argnum (err) t))))))))
    (t (basic-matchp expr form))))

(defmacro matchp (expr form)
  "Determine if the expression matches the form.

Example
  (matchp 3 (type 'number))"
  `(quoted-matchp ,expr ',form))

(defun arglist-count-args (args)
  "Counts the number of parameters in the given argument
specification, not including a &rest argument."
  (cond
    ((null args) 0)
    ((equal '&rest (car args)) 0)
    (t (1+ (arglist-count-args (cdr args))))))

(defun arglist-simplify (args)
  "Remove all argument specifiers (symbols with '&') from args."
  (cond
    ((null args) nil)
    ((equal #\& (char (string (car args)) 0)) 
     (arglist-simplify (cdr args)))
    (t (cons (car args) (arglist-simplify (cdr args))))))

(defun arglist-pos (val args)
  "Return the position of val in args. If val is a &rest argument,
  return a cons cell where the car is the argument position (not the
  list position) and the cdr is a &rest symbol."
  (let ((rest-pos (position '&rest args))
	(pos (position val args)))
    (if (and rest-pos (< rest-pos pos))
	(cons (- pos 1) '&rest)
	pos)))

(defmacro defpattern (name args special-args 
		      &optional doc-string pred &rest binds)
  "Define a new pattern that can be used by matchp. Patterns exist
  in a separate namespace from functions.

name: The name of the pattern. A list may match the pattern if it
  begins with this symbol.
args: An argument specificer much like that for functions. When a 
  pattern is matched against, think of it as calling a function defined
  by defpattern that takes in as arguments each value passed with the
  pattern. The argument specifier may use &rest.
special-args: An argument specifier containing exactly one argument,
  which binds to the expression. By convention, this is given as
  (expr).
doc-string: An optional documentation string describing the pattern.
pred: The body for a predicate function that uses the values passed
  in to args and special-args to determine whether the expression
  matches the pattern.
binds: A list of forms where each form specifies how to bind a 
  variable. A single form contains two elements: first the name of the
  argument, and second an expression that uses the  passed in to 
  args and special-args to return the value of the given variable. 
  a &rest argument, it contains three forms: the first is the same as
  above; the second is a variable name which binds to the number of
  the argument passed in (that is, if there are 3 arguments wrapped
  into the &rest argument, then 0 is passed in to find the value for
  the first argument, 1 for the value of the second, and 2 for the
  value of the third); and the third is the same as
  the second argument given previously.
"
  (let ((argnum-sym (gensym))
	(expected-special-args 1))
    ;; If the pattern definition does not contain a doc string,
    ;; rearrange the arguments.
    (when (not (stringp doc-string))
      (if (not (and (null pred) (null binds)))
	  (setf binds (cons pred binds)))
      (setf pred doc-string)
      (setf doc-string nil))
	
    (if (not (= (length special-args) expected-special-args))
	(error 'defpattern-wrong-number-special-args
	 (format nil
		 "Wrong number of special arguments (~a found, ~a expected)"
		 (length special-args) expected-special-args))
	`(set-pattern ',name
	  (make-instance 'Pattern 
	   :num-args 
	   ,(arglist-count-args args)

	   :doc-string
	   ,doc-string
	   
	   :matchp-test
	   (lambda (,@special-args ,@args) ,pred)

	   :value-getter
	   (lambda (,@special-args ,argnum-sym)
	     (cond
	       ,@(nconc 
		  (mapcar 
		   (lambda (body)
		     (let ((pos (arglist-pos (car body) args)))
		       (cond
			 ((consp pos)
			  `((>= ,argnum-sym ,(car pos))
			    (let ((,(cadr body) (- ,argnum-sym ,(car pos))))
			      ,(caddr body))))
			 ((null pos)
			  (error 'defpattern-unknown-argument :text
				 (format nil 
					 "Pattern ~a has no argument ~a in argument list ~a"
					 name (car body) args)))
			 (t `((= ,argnum-sym ,pos) ,(cadr body))))))
		   binds)
		  `((t (error 'pattern-wrong-argnum :text
			      (format nil "Pattern ~a has no argument number ~a"
				      ',name ,argnum-sym))))))))))))


(defun var-used-p (var to-eval)
  "Determine if the given variable is used in the expression. This
  isn't perfect; if you do something like 
    (var-used-p 'x '(quote x))
  or 
    (var-used-p 'x (x 3)) ; x is a function
  then it will incorrectly say that x is used.
"
  (cond
    ((consp to-eval)
     (or (var-used-p var (car to-eval))
	 (var-used-p var (cdr to-eval))))
    (t (equal var to-eval))))

;; TODO: Do something about unused variables. Search through the
;; expression to see if a variable is ever referenced; if not, don't
;; define it.
(defun bind-vars (expr-sym to-eval form)
  "Treat symbols in FORM as variables. Create a list of variables
in form where each element is a list containing first the variable,
then the expression in expr to which the variable is bound."
  (labels 
      ((rec (form-rec index path)
	 (cond
	   ;; Ignore the function name. All other symbols are
	   ;; considered variables.
	   ((and (= index 0) (consp form-rec))
	    (if (equal (car form-rec) 'quote)
		nil
		(rec (cdr form-rec) 1 path)))
	   ((consp form-rec)
	    (nconc (rec (car form-rec) 0 (cons (1- index) path))
		   (rec (cdr form-rec) (1+ index) path)))
	   ((and (symbolp form-rec) (not (booleanp form-rec))
		 (var-used-p form-rec to-eval))
	    (list (list form-rec 
	      `(get-var-value ,expr-sym ',form ',(reverse path)))))
	   (t nil))))
    (rec form 0 nil)))
	    

(defun get-var-value (expr form path)
  "Find the (num)th variable in FORM and return its corresponding
value in expr. If it does not understand the format of FORM, 
return nil. Notice that this behavior is useful for some cases, but
it means that the function will quietly ignore invalid input."
  (cond
    ((consp form)
     (let ((pattern (get-pattern (car form)))
	   (num (car path)))
       (when (not pattern)
	 (error 'undefined-pattern :text
		(format nil "Undefined pattern ~a" (car form))))
       (let ((next-val (funcall (slot-value pattern 'value-getter)
				expr num)))
	 (if (cdr path)
	     (get-var-value next-val (nth (1+ num) form) (cdr path))
	     next-val))))
    ;; If form is a symbol, it matches all of expr, so bind expr to
    ;; the symbol.
    ((symbolp form) expr)
    (t nil)))


(defmacro add-scope (expr form to-eval)
  "Put the variables in form into scope and bind them to the
  corresponding values in expr. Then evaluate to-eval."
  (let* ((expr-sym (gensym))
	 (var-list (bind-vars expr-sym to-eval form)))
    (if var-list
	`(let ((,expr-sym ,expr))
	  (let ,var-list
	   ,to-eval))
	to-eval)))


(defun expand (val body)
  (when (consp body)
    (let ((form (first (car body))) (to-eval (second (car body))))
      (cons `((matchp ,val ,form)
	      (add-scope ,val ,form ,to-eval))
	    (expand val (cdr body))))))


(defmacro match (val &body body)
  "Format: 
    (match val
      (pred1 body1)
      (pred2 body2)
      ...)
Run through the list of predicates. As soon as some predk is found
  such that (matchp val predk) returns t, stop, evaluate bodyk,
  and return. If no matching pattern is found, return nil.
If predk contained any symbols other than function
  calls, add these variables to the local scope, binding them to
  the value in val that they matched. If you wish to use a generic
  placeholder symbol without binding it to a variable, use T."
  `(cond ,@(expand val body)))

(defmacro defmatch (name &body body)
  "A facility for defining functions that use pattern matching on the
arguments, similarly to Haskell or other functional languages. It
uses the following format: 

    (defmatch name
      \"optional doc-string\"
      forms*)

It takes forms similar to (match), with one key difference. A defmatch
  form may have more than two parts. The last part is the expression,
  and all other parts are matched against the arguments. So whereas in
  a match form you may match two values by bundling them in a list and
  then using the form ((list x y) (do-something x y)), with defmatch
  you can simply pass the two values as arguments and then use the
  form (x y (do-something x y))."
  (let ((arg-sym (gensym))
	(doc-string ""))
    (when (typep (car body) 'string)
      (setf doc-string (car body))
      (setf body (cdr body)))
    `(defun ,name (&rest ,arg-sym)
       ,doc-string
       (match ,arg-sym
	      ,@(mapcar (lambda (x) 
			 `((list ,@(butlast x)) ,@(last x)))
		       body)))))


;; Resume printing style-warning errors.
(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))

;; Load the default patterns.
(load "patterns")
