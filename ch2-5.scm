;;;;; Chapter 2.5 Systems with Generic Operations

(import r7rs)
(import srfi-1)
(import srfi-197)
(load "lib.scm")

;;;; 2.5.1 Generic Arithmetic Operations

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
	 (proc (get op type-tags)))
    (if proc
	(apply proc (map contents args))
	(error "No method for theses types: APPLY-GENERIC" (list op type-tags)))))

(define (install-ordinary-package)
  (define (tag x) (attact-tag 'number x))
  (put 'add '(number number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(number number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(number number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(number number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'number (lambda (x) (tag x)))
  'done)

;;;; 2.5.2 Combining Data of Different Types



;;;; 2.5.3 Example: Symbolic Algebra

function with 1 argument = unary = monadic
function with 1 argument = binary = dyadic
