#lang racket

(define (tagged-list? expr tag)
  (if (pair? expr)
    (eq? (car expr) tag)
    #f))

(define (definition? expr)
  (tagged-list? expr 'define))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(provide
  tagged-list?
  definition?
  make-lambda
  make-let)
