;;;;; Chapter 3.3 Modeling with Mutable Data

(import r7rs)
(import srfi-1)
(import srfi-197)
(load "lib.scm")

;;;; 3.3.1 Mutable List Structure

The primitive mutators for pairs are set-car! and set-cdr!. The first argument of both must be a pair.

;;; Exercise 3.14
The following procedure is quite useful, although obsure.
(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

Mystery is a mutating reverse function.

(define v (list 'a 'b 'c 'd))
(mystery v)
v

;;;; 3.3.2 Representing Queues



;;;; 3.3.3 Representing Tables



;;;; 3.3.4 A Simulator for Digital Circuits



;;;; 3.3.5 Propagation of Constraints
