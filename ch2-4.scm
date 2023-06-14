;;;;; Chapter 2.4 Multiple Representations for Abstract Data

(import r7rs)
(import srfi-1)
(import srfi-197)
(load "lib.scm")

;;;; 2.4.1 Representations for Complex Numbers

TLDR, the way data can be used and the way data can be stored should be separate.
That way refactoring is easy and extensible.

;;;; 2.4.2 Tagged data

The following code attaches a tag to data in the form of a pair with a symbol.
This is useful so we can have a "type system" and handle different kinds of data
that will be passed to the same function easily.
i.e. passing rectangular numbers and polar numbers. You can tag the rectangular numbers
as 'rectangular and the polar numbers as 'polar so the function can easily know what type
of data you have given to it.

(define (attact-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (real-part-polar z)
  (* (magni)))

;;;; 2.4.3 Data-Directed Programming and Additivity

The general strategy of checking the type of a datum and calling an appropriate procedure
is called "dispatching on type". This is a powerful strategy for obtaining modularity in
system design. On the other hand, implementing dispatch has two significant weaknesses.

One weakness is that the generic interface procedures must know about all the different
representations. For instance, suppose we wanted to incorporate a new representation for
complex numbers into our complex number system. We would need to identify this new
representation with a type, and then add a clause to each of the generic interface procedures
to check for the new type and apply the appropriate selector for that representation.

Another weakness of the technique is that even though the indivitual representations can be
designed separately, we must guarentee that no two procedures in the entire system have
the same name.


Alternatively, you can store all functions with respect to a tag in a data structure
(like a hash table). Then you can simply request the appropriate function to be run
by sending the appropriate tag.
