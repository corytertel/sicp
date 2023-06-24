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

(define (make-queue)
  '())

;; Doesn't work
(define (insert-queue! queue x)
  (set! queue (append! queue (list x)))
  queue)

(define (peek-queue queue)
  (car queue))

(define (pop-queue! queue)
  (let ((head (car queue)))
    (set! queue (cdr queue))
    head))

The above definitions make a queue that's essentially alist and appends new items to the end.
This works, but has an O(n) time for insertion. The queue could also be structured the
other way: O(1) for insertion and O(n) for peeking/popping.

Alternatively the queue could be structured differently, with the queue having a pointer
to both the front and rear pointer for an O(1) time for both insertion and peeking.

(define (front-ptr queue)
  (car queue))

(define (rear-ptr queue)
  (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

;; Only needs to check front-ptr bc that's what's storing the whole list.
(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue)
  (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "Front called with an empty queue" queue)
      (car (front-ptr queue))))

This function inserts the new item using the last pointer and then resets the last pointer.
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (else
	   (set-cdr! (rear-ptr queue) new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue))))

(define (pop-queue! queue)
  (cond ((empty-queue? queue) (error "POP! called with an empty queue" queue))
	(else (set-front-ptr! queue (cdr (front-ptr queue)))
	      queue)))

(define q (make-queue))
(insert-queue! q 'a)
(insert-queue! q 'b)
(pop-queue! q)
(insert-queue! q 'c)
(insert-queue! q 'd)
(pop-queue! q)

;;; Exercise 3.21

(define (print-queue queue)
  (display (front-ptr queue)))

;;; Exercise 3.22
A deque ("double ended queue") is a sequence in which items can be inserted and deleted at either the front or the rear. Operations on deques are the constructor make-deque, the predicate empty-deque?, selectors front-deque and rear-deque, mutators front-push-deque!, rear-push-deque!, front-pop-deque!, and rear-pop-deque!. All operation should be accomplished in O(1) steps.

A deque can be easily implemented as a doubly linked list.

Implement a doubly linked list.
Form of elements: '(value list-of-next-elements list-of-prev-elements)

;; Doubly list has a pointer to the front and rear of the list
(define (make-doubly-list)
  (cons #f #f))

;; List is effectively empty if the front pointer is null
(define (doubly-empty? DLST)
  (not (car DLST)))

(define (doubly-append-front! DLST X)
  (let ((NEW-ELEM (list X #f #f)))
    (cond ((doubly-empty? DLST)
	   (set-car! DLST NEW-ELEM)
	   (set-cdr! DLST NEW-ELEM))
	  (else
	   (set! (caddr (car DLST)) NEW-ELEM)
	   (set! (cadr NEW-ELEM) (car DLST))
	   (set-car! DLST NEW-ELEM)))))

(define (doubly-append-rear! DLST X)
  (let ((NEW-ELEM (list X #f #f)))
    (cond ((doubly-empty? DLST)
	   (set-car! DLST NEW-ELEM)
	   (set-cdr! DLST NEW-ELEM))
	  (else
	   (set! (cadr (cdr DLST)) NEW-ELEM)
	   (set! (caddr NEW-ELEM) (cdr DLST))
	   (set-cdr! DLST NEW-ELEM)))))

(define (doubly-front DLST)
  (caar DLST))

(define (doubly-rear DLST)
  (cadr DLST))

(define (doubly-list->list DLST)
  (define (iter ELEM ACC)
    (if (caddr ELEM)
	(iter (caddr ELEM) (cons (car ELEM) ACC))
	(cons (car ELEM) ACC)))
  (iter (cdr DLST) '()))

(define (doubly-list-rear->list DLST)
  (define (iter ELEM ACC)
    (if (cadr ELEM)
	(iter (cadr ELEM) (cons (car ELEM) ACC))
	(cons (car ELEM) ACC)))
  (iter (car DLST) '()))

(define (doubly-list-ref DLST N)
  (define (iter ELEM N)
    (cond ((= N 0) (car ELEM))
	  ((not (cadr ELEM)) #f)
	  (else (iter (cadr ELEM) (- N 1)))))
  (if (< N 0)
      (error "N cannot be negative." N)
      (iter (car DLST) N)))

(define (doubly-list-ref-rear DLST N)
  (define (iter ELEM N)
    (cond ((= N 0) (car ELEM))
	  ((not (caddr ELEM)) #f)
	  (else (iter (caddr ELEM) (- N 1)))))
  (if (< N 0)
      (error "N cannot be negative." N)
      (iter (cdr DLST) N)))

(define d (make-doubly-list))
(doubly-append-front! d 'b)
(doubly-append-front! d 'a)
(doubly-append-rear!  d 'c)
(doubly-front d)
(doubly-rear d)
(doubly-list->list d)
(doubly-list-rear->list d)

;;; TODO do the common lisp alg book, but in scheme using the table of contents for guidance

;;;; 3.3.3 Representing Tables

;; 1-dimensional table from book

(define (lookup KEY TABLE)
  (alist-ref KEY (cdr TABLE)))

(define (insert! KEY VALUE TABLE)
  (set-cdr! TABLE (alist-update KEY VALUE (cdr TABLE))))

(define (make-table)
  (list '*table*))

(define t (make-table))
(insert! 'a 1 t)
(insert! 'b 2 t)
(insert! 'c 3 t)
(insert! 'd 4 t)
t

;; 2-dimensional table
essentially 2 different sub-tables within your table

(define (lookup KEY-1 KEY-2 TABLE)
  (and-let* ((SUBTABLE (assoc KEY-1 (cdr TABLE)))
	     (RECORD (assoc KEY-2 (cdr SUBTABLE))))
    (cdr RECORD)))

(define (insert! KEY-1 KEY-2 VALUE TABLE)
  (let ((SUBTABLE (assoc KEY-1 (cdr TABLE))))
    (if SUBTABLE
	(let ((RECORD (assoc KEY-2 (cdr SUBTABLE))))
	  (if RECORD
	      (set-cdr! RECORD VALUE)
	      (set-cdr! SUBTABLE
			(cons (cons KEY-2 VALUE)
			      (cdr SUBTABLE)))))
	(set-cdr! TABLE
		  (cons (list KEY-1
			      (cons KEY-2 VALUE))
			(cdr TABLE))))))

(define t (make-table))
(insert! 'a 'a 1 t)
(insert! 'a 'b 2 t)
(insert! 'b 'a 3 t)
(insert! 'b 'b 4 t)
(lookup 'a '1 t)
(lookup 'a 'a t)
t

;; message passing style of a 2D table

(define (make-table)
  (let ((LOCAL-TABLE (list '*table*)))
    (define (lookup KEY-1 KEY-2)
      (and-let* ((SUBTABLE (assoc KEY-1 (cdr LOCAL-TABLE)))
		 (RECORD (assoc KEY-2 (cdr SUBTABLE))))
	(cdr RECORD)))
    (define (insert! KEY-1 KEY-2 VALUE)
      (let ((SUBTABLE (assoc KEY-1 (cdr LOCAL-TABLE))))
	(if SUBTABLE
	    (let ((RECORD (assoc KEY-2 (cdr SUBTABLE))))
	      (if RECORD
		  (set-cdr! RECORD VALUE)
		  (set-cdr! SUBTABLE
			    (cons (cons KEY-2 VALUE)
				  (cdr SUBTABLE)))))
	    (set-cdr! LOCAL-TABLE
		      (cons (list KEY-1
				  (cons KEY-2 VALUE))
			    (cdr LOCAL-TABLE))))))
    (define (dispatch M)
      (cond ((eq? M 'lookup) lookup)
	    ((eq? M 'insert!) insert!)
	    (else (error "Unknown operation: TABLE" M))))
    dispatch))

(define t (make-table))
((t 'insert!) 'a 'a 1)
((t 'insert!) 'a 'b 2)
((t 'lookup) 'a '1)
((t 'lookup) 'a 'a)

;;; Exercise 3.25
Generalizing one- and two-dimensional tables, show how to implement a table in which values are stored under different numbers of keys. The lookup and insert! procedures should take as input a list of keys used to access the table.

;; TODO FINISHME

(define (make-table)
  (list '*table*))

(define (lookup . ARGS)
  (define (iter KEYS TABLE)
    (if (null? (cdr KEYS))
	(alist-ref (car KEYS) TABLE)
	(iter (cdr KEYS) (alist-ref (car KEYS) TABLE))))
  (iter (drop-right ARGS 1) (cdr (tail ARGS))))

(define (insert! . ARGS)
  (define (iter KEYS VALUE TABLE)
    (alist-update (car KEYS)
		  (if (null? (cdr KEYS))
		      VALUE
		      (iter (cdr KEYS) VALUE (or (alist-ref (car KEYS) TABLE) '())))
		  TABLE))
  (set! (cdar (last-pair ARGS))
    (iter (drop-right ARGS 2) (car (take-right ARGS 2)) (cdar (last-pair ARGS)))))

(define t (make-table))
(insert! 'c 3 t)
(insert! 'a 'a 1 t)
(insert! 'a 'b 2 t)
(lookup 'a '1 t)
(lookup 'a 'a t)
t

;;;; 3.3.4 A Simulator for Digital Circuits



;;;; 3.3.5 Propagation of Constraints
