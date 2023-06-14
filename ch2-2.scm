;;;;; Chapter 2.2 Hierarchical Data and the Closure Property

(import r7rs)
(import srfi-1)
(import srfi-69) ;; hash-tables
(import srfi-197) ;; pipeline operators
(load "lib.scm")

;;;; 2.2.1 Representing Sequences

;;; Exercise 2.17
Define a procedure last-pair that returns the list that contains
only the last element of a given (nonempty) list.

;; this returns the last pair
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

;; this returns the last element
(define last-pair
  (chain-lambda
   (reverse _)
   (car _)))

(last-pair '(1 2 3))

;;; Exercise 2.18
Define a procedure reverse that takes a list as argument and returns a lits of the
same elements in reverse order.

(define (reverse lst)
  (if (null? lst)
      lst
      (append (reverse (cdr lst)) (list (car lst)))))

(reverse '(1 2 3 4 5))

;; Exercise 2.20
Write a procedure same-parity that takes one or more integers and returns a list of
all the arguments that have the same even-odd parity as the first argument.

(define (same-parity x . ints)
  (chain
   (if (odd? x) odd? even?)
   (filter _ ints)
   (cons x _)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

;; Exercise 2.21
The procedure square-list takes a list of numbers as arguments and returns
a list of the squares of those numbers.

(define (square-list nums)
  (if (null? nums)
      '()
      (cons (square (car nums)) (square-list (cdr nums)))))

(define (square-list nums)
  (map square nums))

(square-list '(1 2 3 4))

;;; Exercise 2.23
The procedure for-each is similar to map. It takes as arguments a procedure
and a list of elements. However, rather than forming a list of the results,
for-each just applies the procedure to each of the elements in turn, from
left-to-right. The values returned by applying the procedure to the elements
are not used at all. for-each is used with procedures that perform an action,
such as printing. For example,
(for-each (lambda (x)
	    (newline)
	    (display x))
	  '(57 321 88))
Give an implementation of for-each.

(define (for-each proc lst)
  (if (null? (cdr lst))
      (proc (car lst))
      (begin (proc (car lst))
	     (for-each proc (cdr lst)))))

;;;; 2.2.2 Hierarchical Structures

;;; Exercise 2.25
Give combinations of cars and cdrs that will pick 7 from each of the following lists:
(1 3 (5 7) 9)
((7))
(1 (2 (3 (4 (5 (6 7))))))

(car (cdaddr '(1 3 (5 7) 9)))
(caar '((7)))
(cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))

;;; Exercise 2.27
Modify your reverse procedure of Exercise 2.18 to produce a deep-reverse procedure
that takes a list as argument and returns as its value the list with its elements reversed
and with all sublists deep-reversed as well.

(define (deep-reverse lst)
  (define (deep-reverse-iter lst acc)
    (if (null? lst)
	acc
	(deep-reverse-iter (cdr lst)
			   (cons (if (list? (car lst))
				     (deep-reverse (car lst))
				     (car lst))
				 acc))))
  (deep-reverse-iter lst '()))

(deep-reverse '(1 2 (3 4) 5))
(deep-reverse '((1 2) (3 4)))

;;; Exercise 2.28
Write a procedure fringe that takes as argument a tree (represented as a list) and returns
a list whose elements are all the leaves of the tree arranged in left to right order.

(define (fringe tree)
  (cond ((null? tree) '())
	((list? (car tree)) (append (fringe (car tree)) (fringe (cdr tree))))
	(else (cons (car tree) (fringe (cdr tree))))))

(fringe '(1 2 (3 4) (5 6) ((7 8))))

;;; Exercise 2.30
Define a procedure square-tree analogous to the square-list procedure of 2.21.

(define (square-tree tree)
  (tree-map square tree))

(square-tree (list 1
		   (list 2 (list 3 4) 5)
		   (list 6 7)))

"Once you start thinking in terms of map... you stop thinking about the
particular control structure or order".
"... it really comes out of APL. It is the really important idea in APL
that you stop thinking about control strucures and you start thinking
about operations on aggregates."

;;;; 2.2.3 Sequences as Conventional Interfaces

(define (sum-odd-squares tree)
  (chain
   (flatten tree)
   (filter odd? _)
   (map square _)
   (sum _)))

(define fib-nums (make-hash-table))
(define (fib n)
  (cond ((<= n 1) n)
	((hash-table-exists? fib-nums n) (hash-table-ref fib-nums n))
	(else
	 (let ((val (+ (fib (- n 1)) (fib (- n 2)))))
	   (hash-table-set! fib-nums n val)
	   val))))

;; finds the first n even fib nums
(define (even-fibs n)
  (define (even-fibs-iter n x acc)
    (cond ((<= n 0) acc)
	  ((even? (fib x)) (even-fibs-iter (- n 1) (+ x 1) (cons (fib x) acc)))
	  (else (even-fibs-iter n (+ x 1) acc))))
  (reverse (even-fibs-iter n 0 '())))

;; of the first n fib nums, returns the ones that are even
(define (even-fibs n)
  (define (next k)
    (if (> k n)
	'()
	(let ((f (fib k)))
	  (if (even? f)
	      (cons f (next (+ k 1)))
	      (next (+ k 1))))))
  (next 0))

(define (even-fibs n)
  (chain
   (range 0 n)
   (map fib _)
   (filter even? _)))

Notice how even though even-fibs is a unique procedure, it can be composed of
similar procedures which will solve the problem just as effectively. These
procedures (map, filter, and accumulate) can be used to solve 90% of problems
as they are general patterns for solving problems.

(even-fibs 20)

(map fib (range 0 20))

;;; Exercise 2.34
Evaluating a polynomial in x at given value of x can be formulated as an accumulation.
We evaluated the polynomial
a_n*x^n + a_n-1*x^n-1 + ... + a1*x + a0
using a well-known algorithm called Horner's rule, which structures the computation as
(... (a_n*x + a_n-1)x + ... + a1)x + a0
In other words, we start with a_n, multiply by x, add a_n-1, multiply by x, and so on until
we reach a0.

(define (horner-eval x coeffecient-sequence)
  (foldr (lambda (a acc) (+ a (* acc x))) 0 coeffecient-sequence))

(horner-eval 2 '(1 3 0 5 0 1))

;;; Exercise 2.36

(foldr - 0 '(1 2 3))

foldr
(- 1 (- 2 (- 3 0)))

foldl
(- (- (- 0 1) 2) 3)

The procedure foldr-n is similar to foldr except that it takes as it's third arg
a list of lists, which are all assumed to have the same number of elements. It applies
the designated accumulation procedure to combine the nth elements of each list together.

(define (foldr-n f z lsts)
  (chain
   (apply zip lsts)
   (map (lambda (x) (foldr f z x)) _)))

(foldr-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

This is actually just zip-with in haskell.

;;; Exercise 2.37
Matrices
| 1 2 3 4 |
| 4 5 6 6 |
| 6 8 8 9 |
can be represented as '((1 2 3 4) (4 5 6 6) (6 7 8 9))

(define (dot-product v w)
  (chain
   (map * v w)
   (foldr + 0 _)))

(define (transpose m)
  (apply zip m))

(define (matrix-*-vector m v)
  (chain
   (lambda (x) (dot-product x v))
   (map _ m)))

(define (matrix-*-matrix m n)
  (let (n* (transpole n))
    (map (lambda (x) (map (lambda (y) (dot-product x y)) n*)) m)))

;;; Exercise 2.40
Define a procedure unique-pairs that, given an integer n, generates the sequence of pairs
(i,j) with 1 <= j < i <= n.

(define (unique-pairs n)
  (chain
   (lambda (i) (map (lambda (j) (cons i j))
	       (range 1 i)))
   (flatmap _ (range* 2 n))))

(unique-pairs 4)

(define (smallest-divisor n)
  (define (smallest-divisor-iter i)
    (cond ((= (remainder n i) 0) i)
	  ((> (* i i) n) n)
	  (else (smallest-divisor-iter (+ i 1)))))
  (smallest-divisor-iter 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (prime-sum-pairs n)
  (filter (lambda (p) (prime? (+ (car p) (cdr p))))
	  (unique-pairs n)))

(prime-sum-pairs 7)

;;; Exercise 2.41
Write a procedure to find all ordered triples of distinct positive integers i, j, and k
less than or equal to a given integer n that sum to a given integer s.

(define (unique-sum-triples n s)
  (chain (make-list 3 (range* 1 n))
      (apply cartesian-product _)
      (filter increasing? _)
      (filter (lambda (x) (= s (sum x))) _)))

(sort (lambda (x y) (and (< (car x) (car y)) (< (cadr x) (cadr y)) (< (caddr x) (caddr y))))
      (unique-sum-triples 17 20))
