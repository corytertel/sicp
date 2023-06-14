
;;; Iterative vs Recursive processes (not functions)

Just a simple sum of a list example
Ofc the idtiomatic solution is probably to just
(foldr + 0 '(1 2 3 4))
but we'll do this as an example

;; Recursive process
(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

;; Iterative process
(define (sum lst)
  (define (sum-iter s lst)
    (if (null? lst)
	s
	(sum-iter (+ s (car lst)) (cdr lst))))
  (sum-iter 0 lst))

(sum '(1 2 3 4))
