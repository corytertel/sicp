;;;; Chapter 1.1

(load "lib.scm")

;; "A LISP programmer knows the value of everything, but the cost of nothing."
;; "It is better to have 100 functions operate on one data structure than 100 functions on 10 data
;; structures"
;; "A programmer should acquire good algorithms and idioms"
;; "So you have all these algorithms at your disposal, learn them - that's very important."
Learn to use the rich standard library instead of hand rolling your for loops and algorithms.
The name of Lisp comes from list processing.

;;; 1.1 Expressions
(+ 1 1)

;;; 1.2 Naming and the Environment

You can use `define` to define functions and variables

;;; 1.3 Evaluating Combinations

Arguments are evaluated first

(+ (- 1 2) (* 3 4))
(+ -1 12)

;;; 1.4 Compound Procedures

You can define functions using other functions.

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)
25

;;; 1.5 The Substitution Model fof Procedure Application

Applicative order is eagerly evaluating your arguments.
Normal order is lazily evaluating your arguments.

;;; 1.6 Conditional Exrpessions and Predicates

(define (abs x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

;; Exercise 1.3
Define a procedure that takes three numbers as arguments and returns the sum of the
squares of the two larger numbers.

My original solution
(define (sum-square-two-largest n1 n2 n3)
  (let ((f (lambda (x y) (square (+ x y)))))
    (cond ((and (> n1 n2) (> n3 n2)) (f n1 n3))
	  ((and (> n2 n1) (> n3 n1)) (f n2 n3))
	  (t (f n1 n2)))))

Alternate solution
(define (sum-square-two-largest lst)
  (sort ))

(sort '(1 2 3))
