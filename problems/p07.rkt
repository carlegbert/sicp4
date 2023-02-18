#lang racket

(define (make-let-expr bindings body)
  (list 'let bindings body))

(define (first-let-bindings expr)
  (car (let-bindings expr)))

(define (rest-let-bindings expr)
  (cdr (let-bindings expr)))

(define (let*->nested-lets expr)
  (if (null? (let-bindings expr))
    (sequence->exp (let-body expr))
    (let->combination
      (make-let-expr
        (list (first-let-bindings expr))
        (let*->nested-lets (make-let-expr
                             (rest-let-bindings expr)
                             (let-body expr)))))))
