;;;;; Chapter 1.3 Formulating Abstractions with Higher-Order Procedures

(import srfi-1)
(import srfi-197)

;;;; 1.3.1 Procedures as Arguments

(define (cube x)
  (* x x x))

;; This sums all the integers in the range together
;; (this is just for example, not idiomatic code)
(define (sum-integers a b)
  (if (> a b)
     0
     (+ a (sum-integers (+ a 1) b))))

;; This sums the cube of all the integers in the range together
(define (sum-cubes a b)
  (if (> a b)
     0
     (+ (cube a)
	(sum-cubes (+ a 1) b))))

;; These two are very similar, very few differences in how they're written

;; You can define a function that is an abstraction of these, that just passes in what's different
;; DRY: don't repeat yourself

(define (sum term a next b)
  (if (> a b)
     0
     (+ (term a)
	(sum term (next a) next b))))

(define (sum-integers a b)
  (sum identity a add1 b))

(define (sum-cubes a b)
  (sum cube a add1 b))

;;; Exercise 1.30
The sum procedure above generates a linear recursion. The procedure can be rewritten so that the
sum is performed iteratively. Show how to do this by filing in the missing expressions in the
the following definition.
(define (sum term a next b)
  (define (iter a result)
    (if <??>
       <??>
       (iter <??> <??>)))
  (iter <??> <??>))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
       result
       (iter (next a) (+ (term a) result))))
  (iter a 0))

;;; Exercise 1.31
Write an analogous procedure called product that returns the product of the values of a function
at points over a given range. Show how to define factional in terms of product.

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (factorial n)
  (product identity 1 add1 n))
;;; Exercise 1.32
Show that sum and product are both special cases of a still more general notion calle accumulate
that combines a collection of terms, using some general accumulation function.

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (sum     term a next b) (accumulate + 0 term a next b))
(define (product term a next b) (accumulate * 1 term a next b))

This is essentially just a reduce/fold.

;;; Approximation of pi
;; (Doesn't work for some reason)
(* 8 (foldl + 0 (map (lambda (x) (/ 1.0 x))
		     (map (lambda (x) (foldl * 1 x))
			  (chunks-of (map (lambda (x) (+ 1 (* x 2)))
					  (range-closed 1 1000))
				     2)))))

(chain
 (range 1 1001)
 (map (lambda (x) (+ 1 (* 2 x))) _)
 (chop _ 2)
 (map (lambda (x) (/ 1.0 (foldl * 1 x))) _)
 (foldl + 0 _)
 (* 8 _))

;;; Exercise 1.29
(define (simpsons-integral a b)
  (let ((h (/ (- b a) n))
	(ylst (range-closed a b)))
    (chain
     (take (cdr ylst) (- (length ylst) 2))
     (chop _ 2)
     (apply zip _)
     (map sum _)
     (+ (car ylst)
	(tail ylst)
	(* 4.0 (first _))
	(* 2.0 (second _)))
     (* (/ h 3.0) _))))

Correct? Incorrect?

"Correct Solution":
(define (simpsons-intergral f a b n)
  (let* ((h (/ (+ b a) n))
	 (k (- (/ n 2) 1))
	 (coefficients (flatten
			(append '(1)
				(make-list k '(4 2))
				'(4 1)))))

    (chain
     (range a (+ b h) h)
     (map f _)
     (zip-with * coefficients _)
     (sum)
     (* (/ h 3.0)))))

;;;; 1.3.2 Constructing Procedures Using lambda

;;; Exercise 1.34
Suppose we define the procedure
(define (f g) (g 2))
Then we have
(f square)
(f (lambda (z) (* z (+ z 1))))

What happens if we (peversely) ask the interpreter to evaluate the
combination (f f)? Explain.

It would not work.
The interperter would attempt to evaluate 2 as a procedure.
2 is not a procedure.

;;;; 1.3.3 Procedures as General Methods

;;; Finding fixed points of functions
A number x is called a fixed point of a function f if x satisfies the equation f(x) = x.
For some functions f we can locate a fixed point by beginning with an inital guess
and applying f repeatedly,
f(x), f(f(x)), f(f(f(x))), ...

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;;; Exercise 1.35
Show that the golden ratio φ is a fixed point of the transformation x -> 1 + 1/x,
and use this fact to compute φ by means of the fixed-point procedure.

(fixed-point (lambda (x) (+ 1.0 (/ 1.0 x))) 1)

;;; Exercise 1.36
Modify fixed-point so that it prints the sequence of approximations it generates, using the
newline and display primitives. Then find a solution to x^x = 1000 by finding a fixed point
of x -> log(1000)/log(x).

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 5)

;;;; 1.3.4 Procedures as Returned Values

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;;; Exercise 1.40
Define a procedure cubic that can be used together with the newtons-method procedure
in expressions of the form
(newtons-method (cubic a b c) 1)
to approximate zeros of the cubic x^3 + ax^2 + bx + c

(define (cubic a b c)
  (lambda (x) (+ (* x x x)
	    (* a x x)
	    (* b x)
	    c)))

;;; Exercise 1.41
Define a procedure twice that takes a procedure of one argument as argument and returns
a procedure that applies the original procedure twice.

(define (twice fun)
  (lambda (x) (fun (fun x))))

((twice add1) 1)
(((twice (twice twice)) add1) 5)

;;; Exercise 1.42
Let f and g be two one-argument functions. The *composition* f after g is defined to be
the function x -> f(g(x)). Define a procedure compose that implements composition.
For example, if inc is a procedure that adds 1 to its argument,
((compose square add1) 6) ; => 49

(define (compose f1 f2)
  (lambda (x) (f1 (f2 x))))

;;; Exercise 1.43
(define (repeated proc n)
  (if (<= n 0)
      (lambda (x) x)
      (compose proc (repeated proc (- n 1)))))

((repeated square 3) 5)

;;; Exercise 1.44
(define dx 1.0)

(define (smooth proc)
  (lambda (x) (avg (proc x)
	      (proc (+ x dx))
	      (proc (- x dx)))))

(map (smooth square) (range 0 10))

(map (repeated (lambda (x) (((repeated smooth 2) square) x)) 2) (range 0 10))

The rights and privileges of first-class citizens:
- To be named by variables
- To be passed as arguments to procedures
- To be returned as values of procedures
- To be incorporated into data structures
