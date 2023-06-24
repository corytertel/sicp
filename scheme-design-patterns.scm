
(import r7rs)
(import srfi-1)
(import srfi-69)
(import srfi-197)

=== Closures ===

You can "close" a value within a function by doing the following.
(let ((x 0))
  (lambda (y) (+ x y)))
The let expression ends and returns the procedure that can access x.
x can no longer be accessed from anywhere else except inside that procedure.
This is how you can do encapsulation in Scheme.

Using this technique, you can create procedures that contain state, but don't "leak" that
state into the global program. This reduces accidents in code and makes the code less confusing.

A great example is a procedure that finds the fibonacci numbers using dynamic programming.

(define fib
  (let ((fib-nums (make-hash-table)))
    (lambda (n)
      (cond ((<= n 1) n)
	    ((hash-table-exists? fib-nums n) (hash-table-ref fib-nums n))
	    (else
	     (let ((val (+ (fib (- n 1)) (fib (- n 2)))))
	       (hash-table-set! fib-nums n val)
	       val))))))

=== Data-Directed Programming ===

Instead of manually changing the definition for a function for every new type the function
needs to support, the function can abstract this functionality away to a data structure
like a hash-table with the use of tags that will return the function necessary for computation.

From:
(define (add x y)
  (cond ((and (number? x) (number? y))
	 (+ x y))
	((and (string? x) (string? y))
	 (concatenate x y))
	((and (list? x) (list? y))
	 (append x y))))

To:
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define add-ht (make-hash-table))
(hash-table-set! add-ht 'number +)
(hash-table-set! add-ht 'string concatenate)
(hash-table-set! add-ht 'list append)

(define (add x y)
  (if (eq? (type-tag x) (type-tag y))
      ((hash-table-ref add-ht (type-tag x)) (contents x) (contents y))
      (error "Types do not match" (type-tag x) (type-tag y))))

(add (cons 'number 2) (cons 'number 3))

Now it is really easy to add new behavior.
This is similar to adding polymorphism/generics.
This uses an "intelligent operation".

This generic pattern works well. But when many generic procedures are needed it can get boilerplate. If there is a lot of generic functions needed over the same types, then this can be expanded
to use a apply-generic function where the key is now a pair of the function name and the type
so the amount of repetitive code is reduced.
i.e. a hash-table named proc-ht would store k,v pairs like:
[('add, 'number), +]
[('sub, 'number), -]
[('add, 'list), append]
[('sub, 'list), remove]

(define (apply-generic op x y)
  (if (eq? (type-tag x) (type-tag y))
      ((hash-table-ref proc-ht (cons op (type-tag x))) (contents x) (contents y))
      (error "Types do not match" (type-tag x) (type-tag y))))
(define (add x y)
  (apply-generic 'add x y))


Intellegent-Data Programming

Another implementation strategy is to use "intelligent data objects".
You arrange data objects as a procedure that takes in an operation name.

(define (make-rectangular x y)
  (define (dispatch op)
    (cond ((eq? op 'x-coordinate) x)
	  ((eq? op 'y-coordinate) y)
	  ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
	  (else (error "Unknown op: MAKE-RECTANGULAR" op))))
  dispatch)

(define point (make-rectangular 1 4))
(point 'x-coordinate)
(point 'y-coordinate)
(point 'magnitude)

This is very similar to the smalltalk object system where an object takes in a message.
"Alan Kay" style object-orientation.


;; MAYBE
The former is better for a function that used with many different data types.
The latter is better for data that's used with many different functions.


Inheritance can also be implemented. Method overriding also works with this.

(define (make-rectangular-child x y)
  (let ((super (make-rectangular x y)))
    (define (dispatch op)
      (cond ((eq? op 'inverse) (make-rectangular-child y x))
	    ((eq? op '->list) (list x y))
	    (else (super op))))
    dispatch))

(define point2 (make-rectangular-child 2 3))
(point2 'x-coordinate)
(point2 'y-coordinate)
(point2 'magnitude)
((point2 'inverse) '->list)


Storing objects as lists is nice for organization, but it might be confusing to use. One possible way is a "combination" of message passing and function calls. Use function calls for "public interface" exposed messages, and internal dispatching for private functions.
Example:

(define (make-rectangular x y)
  (define (square x)
    (* x x))
  (define (dispatch op)
    (cond ((eq? op 'x-coordinate) x)
	  ((eq? op 'y-coordinate) y)
	  ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
	  (else (error "Unknown op: MAKE-RECTANGULAR" op))))
  dispatch)

(define (rectangle-magnitude rect)
  (rect 'magnitude))


;; TODO implement all of the patterns on refactoring guru
