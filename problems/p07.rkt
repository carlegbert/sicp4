#lang racket

(define (make-let-expr bindings body)
  (list 'let bindings body))

(define (let*->nested-lets expr)
  (if (null? (cdr (let-bindings expr)))
    (let->combination expr)
    (let ((outer-let-binding
            (list (car (let-bindings expr))))
          ((inner-let-bindings
             (cdr (let-bindings expr)))))
      (let ((inner-let-body
              (let*->nested-lets
                (make-let-exprs (inner-let-bindings) (let-body expr)))))
        (let->combination
          (make-let-expr outer-let-binding inner-let-body))))))
