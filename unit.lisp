;;;;
;;;; unit.lisp
;;;; ---------
;;;; Created by Michael Dickens.
;;;; 
;;;; Simple unit test framework.
;;;; 


(defpackage :unit
  (:use :common-lisp)
  (:export :check
	   :verbose-on
	   :verbose-off))

(in-package :unit)

(defmacro report-test (test form expected report-success-p)
  "Calls form and expected on test. Prints a message and returns the
  result of the call.

test: A predicate function taking two arguments, typically an equality
function.
form: An expression to evaluate.
expected: The value to which the form is compared.
report-success-p: If t, prints a message upon success or failure. If
nil, only prints a message upon failure.
"
  `(let ((eval-expected ,expected) (eval-found ,form))
     (if (,test eval-expected eval-found)
	 (progn
	   (when ,report-success-p
	     (format t "     pass     : ~a ~a ~a~%" 
		     ',test ',form ',expected))
	   t)
	 (format t "*****FAIL*****: ~a ~a ~a~%~a(found ~a, expected ~a)~%"  
		 ',test ',form ',expected
		 "                "
		 eval-found eval-expected))))

(defmacro check (&body forms)
  "Checks a list of forms.

forms: A list of forms where each individual form contains three
  elements in order: a test function, not quoted (e.g. eql); a form to
  test; and the expected result.

return: t if all tests pass, nil if any tests fail.

When verbose printing is on, this will print every test. When verbose
printing is off, it will only print failures. Verbose printing is on
by default; it may be turned on by writing \"verbose-on\" (without the
quotes) where a form would go, and similarly may be turned off with
\"verbose-off\". If you are using unit as a separate package, you must
use unit:verbose-on or unit:verbose-off.

Comments may also be used: simply put a string where a form would go,
and this will print the string at the same time as it would evaluate
a form in the same place. Comments are printed even when verbose
printing is turned off.
"
  (let ((verbosep t))
  `(reduce (lambda (pred x) (and pred x))
    (list 
      ,@(loop for form in forms collect
	     (cond
	       ((stringp form)
		`(progn (format t "~%~a~%" ,form) t))
	       ((or (equal form 'verbose-on)
		    (equal form ''verbose-on))
		(setf verbosep t)
		`t)
	       ((or (equal form 'verbose-off)
		    (equal form ''verbose-off))
		(setf verbosep nil)
		`t)
	       (t
		`(report-test ,(car form) ,(cadr form)
			      ,(caddr form)
			      ,verbosep)))))
    :initial-value t)))

