;;;;; Chapter 2.3 Symbolic Data

(import r7rs)
(import srfi-1)
(import srfi-197)
(load "lib.scm")

;;;; 2.3.1 Quotation

;;; Exercise 2.54
Two lists are said to be equal? if they contain equal elements arranged in the same order.
For example,
(equal? '(this is a list) '(this is a list))
is true but
(equal? '(this is a list) '(this (is a) list))
is false. To be more precise, we can define equal? recursively in terms of the basic eq?
equality of symbols by saying that a and b are equal?, or if they are both symbols and
the symbols are eq?, or if they are both lists such that (car a) is equal? to (car b) and
(cdr a) is equal? to (cdr b).
Using this idea, implement equal? as a procedure.

(define (equal? a b)
  (or (and (symbol? a) (symbol? b) (eq? a b))
     (and (null? a) (null? b))
     (and (list? a) (list? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))))

;;;; 2.3.2 Example: Symbolic Differentiation

;;; Exercise 2.56
Make a basic differentiator to handle the power rule of calculus.
i.e. u^n -> nu^n-1

(define (power-rule e)
  (cond ((not (and (eq? (car e) 'expt) (number? (caddr e)))) #f)
	((eq? (caddr e) '1) '1)
	((eq? (caddr e) '2) `(* 2 ,(cadr e)))
	(else `(* ,(caddr e) (expt ,(cadr e) ,(- (caddr e) 1))))))

(power-rule '(expt x 1)) ;; => 1
(power-rule '(expt x 2)) ;; => (* 2 x)
(power-rule '(expt x 3)) ;; => (* 3 (expt x 2))
(power-rule '(+ x 4)) ;; => #f
(power-rule '(expt x y)) ;; => #f

;;;; 2.3.3 Example: Representing Sets

(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1) (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

(define (union-set s1 s2)
  (chain (filter (lambda (x) (not (element-of-set? x s1))) s2)
      (append s1 _)))

(intersection-set '(1 2 3 4) '(3 4 5 6))
(union-set '(1 2 3 4) '(3 4 5 6))

;;; Sets as ordered lists

(define (element-of-ordered-set? x set)
  (cond ((null? set) #f)
	((< x (car set)) #f)
	((= x (car set)) #t)
	(else (element-of-ordered-set? x (cdr set)))))

;;; Exercise 2.62
Give a Θ(n) implementation of union-set for sets represented as ordered lists.
(define (union-ordered-set s1 s2)
  (cond ((and (null? s1) (null? s2)) '())
	((null? s1) s2)
	((null? s2) s1)
	((< (car s1) (car s2)) (cons (car s1) (union-ordered-set (cdr s1) s2)))
	((= (car s1) (car s2)) (cons (car s1) (union-ordered-set (cdr s1) (cdr s2))))
	(else (cons (car s2) (union-ordered-set s1 (cdr s2))))))

(union-ordered-set '() '(1 2 3))

;;; Sets as binary trees

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (cadr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-binary-tree-set? x set)
  (cond ((null? set) #f)
	((= x (entry set)) #t)
	((< x (entry set)) (element-of-binary-tree-set? x (left-branch set)))
	((> x (entry set)) (element-of-binary-tree-set? x (right-branch set)))))

;;; Exercise 2.63
Each of the following two procedures converts a binary tree to a list.
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1
		     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list
			     (right-branch tree)
			     result-list)))))
  (copy-to-list tree '()))

(define-alias tree->list-2 tree->list)

(define (list-tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
	     (left-result (partial-tree elts left-size))
	     (left-tree (car left-result))
	     (non-left-elts (cdr left-result))
	     (right-size (- n (+ left-size 1)))
	     (this-entry (car non-left-elts))
	     (right-result (partial-tree (cdr non-left-elts) right-size))
	     (right-tree (car right-result))
	     (remaining-elts (cdr right-result)))
	(cons (make-tree this-entry left-tree right-tree)
	      remaining-elts))))

;; Expensive but whatever
(define (union-binary-tree-set s1 s2)
  (list-tree
   (remove-duplicates
    (append (tree->list a)
	    (tree->list b)))))

;;;; 2.3.4 Example: Huffman Encoding Trees
