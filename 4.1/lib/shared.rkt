#lang racket

(define (tagged-list? expr tag)
  (if (pair? expr)
    (eq? (car expr) tag)
    #f))

(define (definition? expr)
  (tagged-list? expr 'define))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-set! var val)
  (list 'set! var val))

(define unassigned '*unassigned*)

(provide
  tagged-list?
  definition?
  make-lambda
  make-set!
  unassigned)
