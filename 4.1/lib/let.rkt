#lang racket

(define (let-body expr)
  (cddr expr))

(define (let-bindings expr)
  (cadr expr))

(define (binding-name binding)
  (car binding))

(define (binding-value binding)
  (cadr binding))

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(provide let-body
         let-bindings
         binding-name
         binding-value
         make-let)
