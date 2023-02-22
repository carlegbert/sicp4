; (while
;   predicate    ;; callable which returns a boolean
;   expressions) ;; list of expressions

(define (nothing) (cond))

(define (while? expr)
  (tagged-list? 'while expr))

(define (while-predicate expr)
  (cadr expr))

(define (while-expressions expr)
  (caddr expr))

(define (make-while-recursion expr)
  (make-if (while-predicate expr)
           (list 'while-iter)
           (nothing)))

(define (while->combination expr)
    (make-define
      ;; installing a new name into the environment
      ;; seems absolutely horrible, but I'm not really
      ;; sure how else to accomplish this as a derived
      ;; expression. It seems like it would be better
      ;; to just make an eval-while construct.
      'while-iter
      (make-begin
        (while-expressions expr)
        (cons (make-while-recursion expr)))))
