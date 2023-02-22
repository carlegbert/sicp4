#lang racket

(#%require "../lib/eval.rkt")

(define (named-let? expr)
  (not (pair? (cadr expr))))

(define (named-let-var expr)
  (cadr expr))

(define (named-let-bindings expr)
  (caddr expr))

(define (named-let-body expr)
  (cdddr expr))

(define (make-new-named-let-bindings expr)
  (cons (list (named-let-var expr)
              (named-let-body expr))
        (named-let-bindings expr)))

(define (assignments-from-bindings bindings)
  (map cadr bindings))

(define (vars-from-bindings bindings)
  (map car bindings))

;; make a expression, with a new binding added
;; which is named-let-var bound to named-let-body

(define (named-let->combination expr)
  (let ((new-bindings (make-new-named-let-bindings expr)))
    (cons (make-lambda (vars-from-bindings new-bindings)
                       (make-begin (named-let-body expr)))
          (assignments-from-bindings new-bindings))))

(define (let->combination expr)
  (if (named-let? expr)
    (named-let->combination expr)
    (cons (make-lambda (let-vars expr)
                       (make-begin (let-body expr)))
          (let-assignments expr))))
