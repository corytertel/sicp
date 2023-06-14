;;;;; Chapter 3.1 Assignment and Local State

(import r7rs)
(import srfi-1)
(import srfi-197)
(load "lib.scm")


Primitive procedures and primitive data are not sufficient enough for designing effective programs.
Effective programs also require organizational principles that can guide us in the overall design
of a program.

2 main approaches:
- object-based approach
- stream-processing approach

;;;; 3.1.1 Local State Variables

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))

(withdraw 25)

set! should only be used for its effect, not for its value.

Operations that change the values of variables (or change data structures) are given names that
end with an exclamation mark. This is similar ot the convention designating predicates by names
that end with a question mark.

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))))

(new-withdraw 25)

Here the variable balance is encapsulated within the new-withdraw procedure.
Encapsulation reflects the general system-design principle known as the hiding
principle. One can make a system more modular and robus by protecting parts of the
system from each other, that is, by providing information access only to those
parts of the system that have a "need to know". It reduces teh program complexity.

TLDR: if it doesn't need to be at global scope, you can encapsulate it in local scope.

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds")))

Here balance is taken as an argument, and then sets it.

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

(define account (make-account 100))
((account 'withdraw) 25)

Here the withdraw function is owned by an account object, where the data needed to withdraw
is closed within. This is created using dispatching (aka "message passing").

;;; Exercise 3.1
An accumulator is a procedure that is called repeatedly with a single numeric argument and
accumulates its arguments into a sum. Each time it is called, it returns the currently
accumulated sum. Write a procedure make-accumulator that generates accumulators,
each maintaining an independent sum. The input to make-accumulator should specify the
initial value of the sum.

(define (make-accumulator init)
  (let ((sum init))
    (lambda (x)
      (set! sum (+ sum x))
      sum)))

(define a (make-accumulator 5))
(a 10)
(a 10)

;;; Exericse 3.2
In software-testing applications, it is useful to be able to count the number of times a given
procedure is called during the course of a computation. Write a procedure make-monitored that
takes as input a procedure f, that itself takes one input. The result returned by make-monitored
is a third procedure, say mf, that keeps track of the number of times it has been called by
maintaining an internal counter. If the input ot mf is the special symbol how-many-calls?, then
mf returns the value of the counter. If the input is the special symbol reset-count, then mf
resets the counter to zero. For any other input, mf returns the result of calling f on that
input and increments the counter.

(define (make-monitored proc)
  (let ((calls 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) calls)
	    ((eq? x 'reset-count) (set! calls 0))
	    (else (begin (set! calls (+ 1 calls))
			 (proc x)))))))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)

;;; Exericse 3.3
Modify the make-account procedure so thta it creates password-protected accounts. That is,
make-account should take a symbol as an additional argument, as in
(define acc (make-account 100 'secret-password))
The resulting account object should process a request only if it is accompanied by the
password with which the account was created, and should otherwise return a complaint:
((acc 'secret-password 'withdraw) 40)
60
((acc 'some-other-password 'deposit) 50)
"Incorrect password"

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (cond ((not (eq? p password)) (lambda (x) "Incorrect password"))
	  ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

;;; Exercise 3.4
Modify the make-account procedure by adding another local state variable so that if an account
is accessed more than seven consecutive times with an incorrect password, it invokes the procedure
call-the-cops.

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((times-incorrect 0))
    (define (dispatch p m)
      (if (not (eq? p password))
	  (begin (set! times-incorrect (+ 1 times-incorrect))
		 (if (>= times-incorrect 7)
		     (lambda (_) "Calling the cops")
		     (lambda (_) "Incorrect password")))
	  (begin (set! times-incorrect 0)
		 (cond ((eq? m 'withdraw) withdraw)
		       ((eq? m 'deposit) deposit)
		       (else (error "Unknown request: MAKE-ACCOUNT" m))))))
    dispatch))

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)

;;;; 3.1.2 The Benefits of Introducing Assignment

(define rand (let ((x random-init))
	       (lambda () (set! x (rand-update x))
		  x)))

Without assignment, trying to program random is very difficult, if not impossible.

;;;; 3.1.3 The Costs of Introducing Assignment

Programming without any use of assignments is accordingly known as functional programming.

With assignments (mutability), procedures do not return the same value every time, which makes
them unpredictable and hard to use.

A language that supports the concept that "equals can be substituted for equals" in an expression
without changing the value of the expression is said to be referentially transparent. Referential
transparency is violated when we include set! in our computer language. This makes it tricky
to determine when we can simplify expressions by substituting equivalent expressions.
Consequently, reasoning about programs that use assignment becomes drastically more difficult.

In contrast to functional programming, programming that makes extensive use of assignment is
known as imperative programming.

The complexity of imperative programs becomes even worse if we consider applications in which
several processes execute concurrently.

;;; Exercise 3.7

(define (make-joint acc pw new-pw)
  (lambda (p m)
    (if (eq? p new-pw)
	(acc pw m)
	(lambda (_) "Incorrect password"))))

(define acc (make-account 100 'secret-password))
(define acc2 (make-joint acc 'secret-password 'some-other-password))

((acc 'secret-password 'withdraw) 40)
((acc2 'some-other-password 'withdraw) 40)

;;; Exercise 3.8

(define f
  (let ((x 0))
    (lambda (y)
      (set! x (+ x y))
      x)))

(f 0)
(f 1)
