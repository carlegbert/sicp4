#lang racket

(#%require "../lib/frame-table.rkt")

(define (enclosing-environment env)
  (cdr env))

(define (first-frame env)
  (car env))

(define the-empty-environment '())

(define (make-frame variables vals)
  (let ((table (make-table)))
    (define (iter variables vals)
      (if (null? variables)
        table
        (begin
            ((table 'insert!) (car variables) (car vals))
            (iter (cdr variables) (cdr vals)))))
    (iter variables vals)))

(define (lookup-var-in-frame frame var)
  ((frame 'lookup) var))

(define (update-variable-in-frame frame var val)
  ((frame 'set!) var val))

(define (define-variable-in-frame frame var val)
  ((frame 'insert!) var val))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? the-empty-environment env)
      (error "Unbound variable" var)
      (let ((item (lookup-var-in-frame (first-frame env))))
        (if (undefined? item)
          (env-loop (enclosing-environment env))
          item)))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? the-empty-environment env)
      (error "Unbound variable -- SET" var)
      (let ((result (update-variable-in-frame (first-frame env))))
        (if result
          'ok
          (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (define-variable! var val env)
  (define-variable-in-frame var val (first-frame env)))

(provide
  define-variable!
  set-variable-value!
  lookup-variable)
