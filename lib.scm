;; Here are all the "stdlib" functions that i create and use for sicp

(import r7rs)
(import srfi-1)
(import srfi-197)

;; Returns the given list sorted by comp (uses merge sort)
;; pred is a function that takes 2 args and returns which arg it wants to use
(define (sort comp lst)
  (cond ((<= (length lst) 1) lst)
	(else (chain (split lst 2)
		  (map (lambda (x) (sort comp x)) _)
		  (cons comp _)
		  (apply merge _)))))

;; Returns a list of numbers within the range. End is not inclusive.
;; (define (range beg end)
;;   (cond
;;    ((< beg end)
;;     (cons beg (range (add1 beg) end)))
;;    ((> beg end)
;;     (cons beg (range (sub1 beg) end)))
;;    (#t '())))

;; (define (range-closed beg end)
;;   (cond
;;    ((< beg end)
;;     (range beg (add1 end)))
;;    ((> beg end)
;;     (range beg (sub1 end)))
;;    (#t '())))

;; (define (range . args)
;;   (let* ((beg (if (>= (length args) 2) (car args) 0))
;; 	 (end (if (>= (length args) 2) (cadr args) (car args)))
;; 	 (step (cond ((>= (length args) 3) (caddr args))
;; 		     ((< beg end) 1)
;; 		     ((> beg end) -1)
;; 		     (else 0))))
;;     (if (= beg end)
;; 	'()
;; 	(cons beg (range (+ beg step) end step)))))

;; (define (range* . args)
;;   (let* ((beg (if (>= (length args) 2) (car args) 0))
;; 	 (end (if (>= (length args) 2) (cadr args) (car args)))
;; 	 (step (cond ((>= (length args) 3) (caddr args))
;; 		     ((< beg end) 1)
;; 		     ((> beg end) -1)
;; 		     (else 0))))
;;     (range beg (+ end step) step)))

(define (range-iter num step count acc)
  (if (<= count 0)
      acc
      (range-iter (+ num step) step (- count 1) (cons num acc))))

(define (range . args)
  (let* ((beg (if (>= (length args) 2) (car args) 0))
	 (end (if (>= (length args) 2) (cadr args) (car args)))
	 (step (cond ((>= (length args) 3) (caddr args))
		     ((< beg end) 1)
		     ((> beg end) -1)
		     (else 0))))
    (if (= beg end)
	(list beg)
	(reverse (range-iter beg step (/ (- end beg) step) '())))))

(define (range* . args)
  (let* ((beg (if (>= (length args) 2) (car args) 0))
	 (end (if (>= (length args) 2) (cadr args) (car args)))
	 (step (cond ((>= (length args) 3) (caddr args))
		     ((< beg end) 1)
		     ((> beg end) -1)
		     (else 0))))
    (if (= beg end)
	(list beg)
	(reverse (range-iter beg step (+ 1 (/ (- end beg) step)) '())))))

;; Takes a num and a list of nums, then returns a list of list of numbers
;; i.e. (sliding 2 '(1 3 3 1)) returns '((1 3) (3 3) (3 1))
;; Useful for getting lists of each element and their "next" values
;; (define (sliding num lst)
;;   (define (sliding-iter num lst count)
;;     (if (<= count 0)
;; 	'()
;; 	(cons (take num lst) (sliding-iter num (cdr lst) (- count 1)))))
;;   (sliding-iter num lst (- (length lst) (sub1 num))))
(define (sliding num lst)
  (define (sliding-iter num lst count acc)
    (if (<= count 0)
	(reverse acc)
	(sliding-iter num
		      (cdr lst)
		      (sub1 count)
		      (cons (take lst num) acc))))
  (sliding-iter num lst (- (length lst) (sub1 num)) '()))

;; NOTE make-list already does this
;; ;; Returns a list of the thing specified n times
;; (define (repeat x n)
;;   (define (repeat-iter thing count acc)
;;     (if (<= count 0)
;; 	acc
;; 	(repeat-iter thing (- count 1) (cons thing acc))))
;;   (repeat-iter x n '()))

;; Returns a random number between 0 (inclusive) and 1 (exclusive).
(define random
  (let ((a 69069) (c 1) (m (expt 2 32)) (seed 19380110))
    (lambda new-seed
      (if (pair? new-seed)
          (set! seed (car new-seed))
          (set! seed (modulo (+ (* seed a) c) m)))
      (/ seed m))))

;; Combines the results of a map with and
(define (andmap pred lst)
  (letrec ((and-l
	    (lambda x
	      (cond ((null? x) #t)
		    ((car x) (apply and-l (cdr x)))
		    (else #f)))))
    (apply and-l (map pred lst))))

;; Combines the results of a map with or
(define (ormap pred lst)
  (letrec ((or-l
	    (lambda x
	      (cond ((null? x) #f)
		    ((car x) #t)
		    (else (apply or-l (cdr x)))))))
    (apply or-l (map pred lst))))

;; Combines the results of a map with one application of flatten
(define (flatmap proc lst)
  (chain (map proc lst)
      (foldl append '() _)))

;; Split list into evenly sized chunks
;; (define (chunks-of lst size)
;;   (cond ((<= size (length lst)) (cons (take lst size) (chunks-of (drop lst size) size)))
;; 	((null? lst) '())
;; 	(else (list lst))))

;; Sum
(define (sum lst)
  (foldl + 0 lst))

;; Product
(define (product lst)
  (foldl * 1 lst))

;; Returns a new list, combining the matching pairs of each list with fun.
(define (map* fun lsts)
  (chain (apply zip lsts)
      (map (lambda (l) (apply fun l)) _)))

;; Partitions the list into n parts
(define (split lst n)
  (chop lst (ceiling (/ (length lst) n))))

;; Merge two sorted lists, order is based on comp
(define (merge comp l1 l2)
  (cond ((null? l1) l2)
	((null? l2) l1)
	((comp (car l1) (car l2)) (cons (car l1) (merge comp (cdr l1) l2)))
	(else (cons (car l2) (merge comp l1 (cdr l2))))))

;; Return the heads of all lists
(define (heads . lsts)
  (filter-map (lambda (x) (if (null? x)
			 #f
			 (car x)))
	      lsts))

;; Swaps the a'th and b'th elements of the list
(define (swap a b lst)
  (cond ((> a b) (swap b a lst))
	((= a b) lst)
	(else (append (take lst a)
		      (list (list-ref lst b))
		      (take (list-tail lst (+ a 1)) (- b a 1))
		      (list (list-ref lst a))
		      (list-tail lst (+ b 1))))))

;; Returns the element given  that the comp function determines most fit
(define (choose comp . elems)
  (foldl (lambda (a b) (if (comp a b) a b))
	 (car elems)
	 (cdr elems)))

;; Macro for creating aliases
(define-syntax define-alias
  (syntax-rules ()
    ((_ old-name new-name)
     (define-syntax new-name
       (syntax-rules ()
         ((_ . args) (old-name . args)))))))

;; Returns the last element of a list
(define (tail lst)
  (list-ref lst (- (length lst) 1)))

;; Squares x
(define (square x)
  (* x x))

;; Cubes x
(define (cube x)
  (* x x x))

;; Average
(define (avg . vals)
  (/ (sum vals) (length vals)))

;; Iterates over every value in a data structure applying proc and keeping the structure.
(define (tree-map proc tree)
  (map (lambda (x) (if (list? x)
		  (tree-map proc x)
		  (proc x)))
       tree))

;; Returns a list with every combination of the lists given
(define (cartesian-product . lsts)
  (if (null? (cdr lsts))
      (map list (car lsts))
      (flatmap (lambda (x) (map (lambda (y) (cons y x)) (car lsts)))
	       (apply cartesian-product (cdr lsts)))))

;; Returns #t if the list is in increasing order, #f if not
(define (increasing? lst)
  (apply < lst))

;; Returns #t if the list is in decreasing order, #f if not
(define (decreasing? lst)
  (apply > lst))

;; Returns the given lst without any duplicates
(define (remove-duplicates lst)
  (if (null? lst)
      '()
      ((if (member (car lst) (cdr lst))
	   identity
	   (lambda (x) (cons (car lst) x)))
       (remove-duplicates (cdr lst)))))

;; Return the list with n elements shifted left and wrapped around
(define (rotate-l lst n)
  (append (drop lst n) (take lst n)))

;; Return the list with n elements shifted right and wrapped around
(define (rotate-r lst n)
  (append (take-right lst n) (drop-right lst n)))

;; Returns all the subsequences in a list
(define (subsequences lst)
  (if (null? lst)
      '()
      (let ((res (subsequences (cdr lst))))
	(append (list (list (car lst)))
		(map (lambda (x) (cons (car lst) x)) res)
		res))))

;; Removes consecutive equal elements
(define (unique lst)
  (cond ((null? (cdr lst)) lst)
	((equal? (car lst) (cadr lst))
	 (unique (cdr lst)))
	(else
	 (cons (car lst) (unique (cdr lst))))))
