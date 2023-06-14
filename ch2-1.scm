;;;;; Chapter 2.1 Introduction to Data Abstraction

(import srfi-1)
(import srfi-197)

A pure function is by definition referentially transparent.
But a referentially transparent function doesn't necessarily mean it's pure.

;;;; 2.1.1 Example: Arithmetic Operations for Rational Numbers

;; Helper functions for this section
(define (add-rat x y)
  (make-rat (+ (* (denom y) (numer x))
	       (* (denom x) (numer y)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (denom y) (numer x))
	       (* (denom x) (numer y)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (demon y))
	    (* (demon x) (numer y))))

(define (equal-rat?)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (rat? x)
  (and (pair? x) (integer? (car x)) (integer? (cdr x))))

(define (print-rat x)
  (display (/ (numer x) (denom x))))

;; Constructor
(define (make-rat n d)
  (if (< d 0)
      (make-rat (- n) (- d))
      (let ((x (gcd n d)))
	(cons (/ n x) (/ d x)))))

;; Selectors
(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

In this situation (but not all) you can implement rat like this
(define make-rat cons)
(define numer car)
(define denom cdr)
This is essentially making aliases.
This enables data abstraction and refactorability.

(define one-half (make-rat 1 2))

(print-rat one-half)

;;;; 2.1.2 Abstraction Barriers

;;; Exercise 2.2
(define make-point cons)
(define point-x car)
(define point-y cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)
(define (segment-points seg)
  (list (list (caar seg)
	      (cdar seg))
	(list (cadr seg)
	      (cddr seg))))

(define (midpoint-segment seg)
  (make-point (avg (point-x (start-segment seg))
		   (point-x (end-segment seg)))
	      (avg (point-y (start-segment seg))
		   (point-y (end-segment seg)))))

(define (segment-length seg)
  (chain
   (segment-points seg)
   (apply zip _)
   (map (lambda (x) (apply - x)) _)
   (map square _)
   (apply + _)
   (sqrt _)))

;;; Exercise 2.3
(define (make-rectangle x y w h)
  (let ((p1 (make-point x y))
	(p2 (make-point (+ x w) y))
	(p3 (make-point x (+ y h)))
	(p4 (make-point (+ x w) (+ y h))))
    (list (make-segment p1 p2)
	  (make-segment p1 p3)
	  (make-segment p2 p4)
	  (make-segment p3 p4))))

(define rectangle-perimeter
  (chain-lambda
   (map segment-length _)
   (sum _)))

(define rectangle-area
  (chain-lambda
   (take _ 2)
   (map segment-length _)
   (product _)))

(rectangle-perimeter (make-rectangle 2 3 5 6))
(rectangle-area (make-rectangle 2 3 5 6))

;;;; 2.1.3 What is Meant by Data?

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(car (cons 1 2))

;;; Exercise 2.5
Show that we can represent pairs of nonnegative integers using only numbers and
arithmetic operations if we represent the pair a and b as the integer that is
the product 2^a3^b. Give the corresponding definitions of the procedures cons,
car, and cdr.

(define (cons-n a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car-n x)
  (if (= 0 (modulo x 2))
      (add1 (car-n (/ x 2)))
      0))

(define (cdr-n x)
  (if (= 0 (modulo x 3))
      (add1 (cdr-n (/ x 3)))
      0))

(car-n (cons-n 3 44))
(cdr-n (cons-n 3 44))

;;;; 2.1.4 Extended Exercise: Interval Arithmetic
