#lang racket

; (let <- if let-expr is the entire let expression, 'let is (car let-expr)
;   ((varname value-expr) <- (cadr let-expr); list of lists
;    (varname-n value-expr-n)) <- next element (a list) in the list at (cadr let-expr)
;   (body) <- (caddr let-expr) is the first expression to be evaluated as part of the let body.
;          <- (cddr let-expr) is the pointer to the list of all the expressions in the let body.
;   (body-n))

(define (let? expr)
  (tagged-list expr 'let))

(define (let-bindings let-expr)
  (cadr let-expr))

(define (let-vars let-expr)
  (map car (let-bindings let-expr)))

(define (let-assignments let-expr)
  (map cadr (let-bindings let-expr)))

(define (let-body let-expr)
  (cddr let-expr))

(define (let->combination expr)
  (cons (make-lambda (let-vars expr)
                     (make-begin (let-body expr)))
        (let-assignments expr)))

(define (eval exp env)
  (cond ...
        ((let-expr? exp)
         (eval (let->expr exp) env))
        ...))
