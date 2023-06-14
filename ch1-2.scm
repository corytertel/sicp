;;;; Chapter 1.2

(import srfi-1)

;;; Factorial

;; Recursive solution
;; Space and runtime complexity is linear
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))
;; Recursive because fact calls itself within the expression (not at top level)

;; Iterative solution
;; Runtime complexity is linear, but space is constant
(define (fact n)
  (define (fact-iter acc n)
    (if (= 0)
	acc
	(fact-iter (* n acc) (- n 1))))
  (fact-iter 1 n))
;; Iterative because fact-iter calls itself at the top level of the expression

;; Composed of algorithms solution
(define (fact n)
  (foldl * 1 (range 1 (add1 n))))

;;; Fibonaci sequence

;; Recursive
(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

;; Iterative
(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

;; Dynamic programming version
;; (somehow slower? i'll have to debug)
(define (fib n)
  (define fib-alist '())
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (or (alist-ref (- n 1) fib-alist)
		    (begin
		      (set! fib-alist (alist-update (- n 1) (fib (- n 1)) fib-alist))
		      (alist-ref (- n 1) fib-alist)))
		 (or (alist-ref (- n 2) fib-alist)
		    (begin
		      (set! fib-alist (alist-update (- n 2) (fib (- n 2)) fib-alist))
		      (alist-ref (- n 2) fib-alist)))))))

;;; Exercise 1.11
A function f is defined by the rule that
f(n) = n if n < 3, f(n-1)+2f(n-2)+3f(n-3) if n>=3

;; Recursive
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))))

(map f (range 0 10))

;; Iterative
(define (f n)
  ;; a b c are accumulator values, count is what is reduced to zero
  ;; The reason why this works is a b c are the previous 3 values.
  ;; You only need to keep track of the prevous 3 values.
  ;; You move the values down and calculate the new value.
  ;; Think of it as "for the next number you'll need these 3 values"
  (define (f-iter a b c count)
    (if (= count 0)
	a
	(f-iter b
		c
		(+ c
		   (* 2 b)
		   (* 3 a))
		(- count 1))))
  (f-iter 0 1 2 n))

;;; Exercise 1.12
The following pattern of numbers is called a Pascal's triangle.
1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
...
The numbers at the edge of the triangle are all 1, and each number inside the triangle
is the sum of the two numbers above it. Write a procedure that computes elements of
Pascal's triangle by means of a recursive process.

(define (pascal-triangle level)
  (cond
   ((= level 0) '(1))
   ((= level 1) '(1 1))
   (else (cons 1
	       (pascal-triangle-adder (pascal-triangle (- level 1)))))))

(define (pascal-triangle-adder lst)
  (cond
   ((null? lst) '())
   ((null? (cdr lst)) (cons (car lst) '()))
   (else (cons (+ (car lst) (cadr lst))
	       (pascal-triangle-adder (cdr lst))))))

(define (pascal-triangle n)
  (if (= 1 n)
      '(1)
      (pt-iter '((1 1) (1)) (- n 2))))

(define (pt-iter acc n)
  (if (= 0 n)
      (reverse acc)
      (pt-iter (cons (pt-next-row (car acc)) acc) (- n 1))))

(define (pt-next-row row)
  (append '(1) (map (lambda (x) (foldl + 0 x)) (sliding 2 row)) '(1)))

(map pascal-triangle (range 0 10))

;;; Time and Space Complexity

;;; Exponentiation

;; Recursive
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;; Iterative
(define (expt b n)
  (define (expt-iter b n acc)
    (if (<= n 0)
	acc
	(expt-iter b (- n 1) (* b acc))))
  (expt-iter b n 1))

;; Algorithms
(define (expt b n)
  (foldl * 1 (repeat b n)))

(expt 2 8)

;;; Fast Exponentiation

(define (square x) (* x x))

;; Recursive
(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(fast-expt 2 8)

;;; Exercise 1.16
Design a procedure that evolves an interative exponentiation process that uses
sucessive squaring and uses a logarithmic number of steps, as does fast-expt.

;; (define (fast-expt b n)
;;   (define (fast-expt-iter base count acc)
;;     (cond ((= count 0) acc)
;; 	  ((even? count) (fast-expt-iter base (/ count 2) (square acc)))
;; 	  (else (fast-expt-iter base (- count 1) (* base acc)))))
;;   (fast-expt-iter b n 1))
(define (fast-expt b n)
  (define (iter squares extra N)
    (cond ((= N 1) (* squares extra))
	  ((even? N) (iter (square squares) extra (/ N 2)))
	  (else (iter squares (* extra squares) (- N 1)))))
  (iter b 1 n))

(fast-expt 2 8)

;;; Exercise 1.17
Design a multiplication procedure analogous to fast-expt that uses a logarithmic number of steps.

(define (double x)
  (+ x x))

;; (define (halve x)
;;   (if (>= x 2)
;;       (add1 (halve (- x 2)))
;;       0))

(define (halve x)
  (define (halve-iter num acc)
    (if (>= num 2)
	(halve-iter (- num 2) (add1 acc))
	acc))
  (halve-iter x 0))

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

;; does not work with negatives
(define (* a b)
  (cond ((= b 0) 0)
	((even? b) (double (* a (halve b))))
	(else (+ a (* a (- b 1))))))

;; works with negatives
(define (* a b)
  (cond ((and (< a 0) (< b 0)) (* (- a) (- b)))
	((< a 0) (- (* (- a) b)))
	((< b 0) (- (* a (- b))))
	((= b 0) 0)
	((even? b) (double (* a (halve b))))
	(else (+ a (* a (- b 1))))))

;; iterative
(define (* a b)
  (define (*-iter a b acc)
    (cond ((= b 0) (+ acc))
	  ((even? b) (*-iter (double a) (halve b) acc))
	  (else (*-iter a (- b 1) (+ a acc)))))
  (cond ((and (< a 0) (< b 0)) (*-iter (- a) (- b) 0))
	((< a 0) (*-iter (- a) b 0))
	((< b 0) (*-iter a (- b) 0))
	(else (*-iter a b 0))))

(map (lambda (p) (equal? (apply * (car p)) (cdr p)))
     '(((2 2) . 4)
       ((2 4) . 8)
       ((3 3) . 9)
       ((27 2) . 54)))

;;; Greatest common divisor

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;; Primes

(define (smallest-divisor n)
  (define (smallest-divisor-iter i)
    (cond ((= (remainder n i) 0) i)
	  ((> (* i i) n) n)
	  (else (smallest-divisor-iter (+ i 1)))))
  (smallest-divisor-iter 2))

(define (prime? n)
  (= (smallest-divisor n) n))

;;; Exercise 1.27
Demonstrate that the Carmichael numbers really do fool the Fermat test.

;; (define (congruent? a b n)
;;   (equal? (modulo a n) (modulo b n)))

;; (define (fermat-test n)
;;   (let ((a (add1 (floor (* (random) (sub1 n))))))
;;     (congruent? (expt a n) (modulo a n) n)))

;; (map fermat-test '(561 1105 1729 2465 2821 6601))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder
	  (square (expmod base (/ exp 2) m))
	  m))
	(else
	 (remainder
	  (* base (expmod base (- exp 1) m))
	  m))))

(define (fermat-test n a)
  (= (expmod a n n) a))

(define (is-carmichael-number? n)
  (define (fermat-test-all n)
    (andmap (lambda (a) (fermat-test n a)) (range 1 n)))
  (and (fermat-test-all n) (not (prime? n))))

;; Create list of Carmichael numbers through numbers that don't pass the fermat test
(map is-carmichael-number? '(561 1105 1729 2465 2821 6601))

It's important, by the way, to get names for the parts of expressions.
One of the things that every sorceror will tell you is if you have the name
of a spirit, you have power over it. So you have to learn these names so
that we can discuss these things.
-Lecture 1B: Procedures and Processes: Substitution Model

;; For bitwise stuff

(import (chicken bitwise))

;; (load "lib.scm")

(bitwise-and 101 1)
