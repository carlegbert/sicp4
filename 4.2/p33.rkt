#lang sicp

(define (primitive-list-to-lambda-list items)
  (if (null? pair)
    '()
    (map
      (lambda (x)
        ;; let's assume that the lambda-based pair functions
        ;; were defined as lambda-<f> & installed into the
        ;; global environment, e.g. `(list 'cons lambda-cons)`,
        ;; so that we can continue to use the underlying cons
        ;; et. al. in the evaluator.
        (lambda-cons
          (car x)
          (quoted-list-to-lambda-list (cdr x)))))))

(define (value-of-quotation expr)
  (if (list? (cadr expr))
    (primitive-list-to-lambda-list expr)
    (text-of-quotation expr)))

;; new eval clause:
((quoted? expr) (value-of-quotation expr))
