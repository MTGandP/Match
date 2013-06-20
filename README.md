Match
=====
A lightweight, extensible pattern matching library for Common Lisp
------------------------------------------------------------------

Created by Michael Dickens.

See the .lisp files for full documentation of individual macros and
functions.

Installation
------------

To install Match, simply clone the repository and begin using the
library. Load Match by putting `(load "path/to/match.lisp")` in your
Lisp file.

All of the macros and functions in `match.lisp` are defined in the
package `:match`. If you want to use them without adding the `match:`
prefix, you must `import` them.

Features
---------

### Match Predicate `matchp`

Use `matchp` to determine whether an expression matches a form. If you
write new patterns using `defpattern`, `matchp` automatically knows
how to use these patterns.

`matchp` implements the core functionality of Match, but it isn't very
useful by itself. The real benefits come from `match`.

### Match Form `match`

`match` provides a structure similar to `cond` or `case`: given an
expression and a series of forms, it attempts to match the expression
against each form. If it finds a match, it executes a given
expression. On the surface, it looks very similar to `case`. But the
functionality of `match` goes beyond `case` in that it allows binding
new variables: a variable used in a match form is bound to the
corresponding value in the expression. This functionality may be used
to greatly simplify code that would otherwise be complicated.

Consider a simple example: the recursive implementation of the
factorial function that we all know and love: 

    (defun factorial (n)
      (if (= n 0)
          1
          (* n (factorial (1- n)))))

In Haskell, we might implement `factorial` using pattern matching: 

    factorial :: Int -> Int
    factorial 0 = 1
    factorial n = n * (factorial (n - 1))

The Match library allows us to do something similar using Lisp.

    (defun factorial (x)
      (match x
        (0 1)
        (n (* n (factorial (1- n))))))

Using the `defmatch` macro, we can simplify this further: 

    (defmatch factorial
      (0 1)
      (n (* n (factorial (1- n)))))

Note: `match` sometimes automatically initializes variables that may
be used in each form. If you do not use these variables, you may get a
style warning to that effect.

Feedback
--------

I would love to hear any comments you may have about Match. I'm also
interested to hear if you add any functionality or write interesting
new patterns. Leave a comment on the project's GitHub page:
https://github.com/MTGandP/Match
