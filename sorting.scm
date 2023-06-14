
;; Quick sort
(define (quick-sort fun lst)
  )

;; Counting sort
(define (counting-sort fun lst)
  )

;; Pigeonhole sort
;; A sorting algorithm that is suitable for sorting lists of elements where the number of elements
;; and the number of possible key values are approximately the same.
(define (pigeonhole-sort lst)
  (let ((range (- (max lst) (min lst) -1)))
    (define (pigeonhole-sort-iter sorted)
      ())
    (pigeonhole-sort-iter (repeat #f (length lst)))))

;; Gravity sort
(define (gravity-sort fun lst)
  )
