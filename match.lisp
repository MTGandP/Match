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
  (:export :matchp
	   :match
	   :patternp
	   :defpattern))

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
   (matchp-test :initarg :matchp-test)
   (value-getter :initarg :value-getter)))

(defparameter *patterns* (make-hash-table)
  "The keys are symbols representing the names of patterns and the
  values are objects of type Pattern.")

(defun patternp (name)
  "Determines if the given symbol defines a pattern."
  (gethash name *patterns*))

;; TODO: Some parts of this have not been tested.
(defun basic-match-p (expr form)
  "Determines if the expression matches the form. Only uses certain
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


(defun matchp (expr form)
  "Determines whether the expression matches the form."
  (cond
    ((and (consp form) (symbolp (car form)))
     (let ((pattern (gethash (car form) *patterns*)))
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
		     (matchp 
		      (funcall (slot-value pattern 'value-getter)
			       expr i)
		      (nth i (cdr form)))
		   (pattern-wrong-argnum (err) t))))))))
    (t (basic-match-p expr form))))

(defun arglist-count-args (args)
  "Counts the number of parameters in the given argument specification."
  (cond
    ((null args) 0)
    ((equal '&rest (car args)) 0)
    (t (1+ (arglist-count-args (cdr args))))))

(defun arglist-pos (val args)
  "Returns the position of val in args. If val is a &rest argument,
  returns a cons cell where the car is the argument position (not the
  list position) and the cdr is a &rest symbol."
  (let ((rest-pos (position '&rest args))
	(pos (position val args)))
    (if (and rest-pos (< rest-pos pos))
	(cons (- pos 1) '&rest)
	pos)))

;; TODO: if an argument's value is used, don't allow the pattern to
;; assign it to a variable, and vice versa.
;; TODO: Allow for a documentation string.
(defmacro defpattern (name args special-args pred &rest binds)
  "Defines a new pattern that can be used by matchp. Patterns do not
exist in the same namespace as functions.

name: The name of the pattern. A list may match the pattern if it
  begins with this symbol.
args: An argument specificer much like that for functions. When a
pattern is matched against, think of it as calling a function defined
by defpattern that takes in as arguments each value passed with the
pattern. The argument specifier may use &rest.
special-args: An argument specifier containing exactly one argument,
  which binds to the expression. By convention, this is given as
  (expr).
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
  (if (not (= (length special-args) expected-special-args))
    (error 'defpattern-wrong-number-special-args
      (format nil
        "Wrong number of special arguments (~a found, ~a expected)"
	(length special-args) expected-special-args))
    `(setf (gethash ',name ,*patterns*)
     (make-instance 'Pattern 
      :num-args 
      ,(arglist-count-args args)
      
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


;;
;; TODO: Make every nontrivial pattern user-defined instead of
;; built-in. Then use this to simplify the definitions of (bind-vars)
;; and (get-var-value). In the new (get-var-value), count the
;; arguments until reaching the (num)th argument and return its value.
;;


(defun bind-vars (expr form)
  "Treats symbols in form as variables. Creates a list of variables
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
	   ((and (symbolp form-rec) (not (booleanp form-rec)))
	    (list (list form-rec 
	      `(get-var-value ,expr ',form ',(reverse path)))))
	   (t nil))))
    (rec form 0 nil)))
	    

;; TODO: Do something about unused variables.
(defun get-var-value (expr form path)
  "Finds the (num)th variable in form and returns its corresponding
value in expr. If it does not understand the format of form, it
returns nil. Notice that this behavior is useful for some cases, but
it means that the function will quietly ignore invalid input."
  (cond
    ((and (consp expr) (consp form))
     (let ((pattern (gethash (car form) *patterns*))
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
  "Puts the variables in form into scope and binds them to the
  corresponding values in expr. Then evaluates to-eval."
  (let ((var-list (bind-vars expr form)))
    (if var-list
	`(let ,var-list
	   ,to-eval)
	to-eval)))


(defun expand (val body)
  (when (consp body)
    (let ((form (first (car body))) (to-eval (second (car body))))
      (cons `((matchp ,val ',form)
	      (add-scope ,val ,form ,to-eval))
	    (expand val (cdr body))))))

(defun quote-forms (body)
  "Recursive function to put quotes in the right places in each form
  in body."
  (when (consp body)
    (cons (list `',(first (car body))
		(second (car body)))
	  (quote-forms (cdr body)))))

;; TODO: temporarily broken
(defmacro matchq (val &body body)
  "A more verbose but more flexible version of (match). This version
  will strictly evaluate predicates and bodies. For example: 
    (match val
      ((fun x y) ...)) ; no good--it will evaluate (fun x y)
    (match val
      ('(fun x y) ...)) ; this is what you want to do"
  `(cond ,@(expand val body)))


(defmacro match (val &body body)
  "Format: 
    (match val
      (pred1 body1)
      (pred2 body2)
      ...)
Runs through the list of predicates. As soon as it finds some predk
  such that (matchp val predk) returns t, it stops, evaluates bodyk,
  and returns. If predk contained any symbols other than function
  calls, it adds these variables to the local scope, binding them to
  the value in val that they matched. If you wish to use a generic
  placeholder symbol without binding it to a variable, use T."
  `(cond ,@(expand val body)))
;  `(cond ,@(expand val (quote-forms body))))

;; TODO: allow doc strings
(defmacro defmatch (name &body body)
  "A facility for defining functions that use pattern matching on the
arguments, similarly to Haskell or other functional languages."
  (let ((arg-sym (gensym)))
    `(defun ,name (&rest ,arg-sym)
       (match ,arg-sym
	      ,@(mapcar (lambda (x) 
			 `((list ,@(butlast x)) ,@(last x)))
		       body)))))


;; Resume printing style-warning errors.
(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))
