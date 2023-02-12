#lang racket

(define (left-to-right-list-of-values exp env)
  (if (no-operands? exps)
    '()
    (let ((left-op) (eval (first-operand exps) env))
      (cons left-op
            (left-to-right-list-of-values (rest-operands exps) env)))))

(define (right-to-left-list-of-values exp env)
  (if (no-operands? exps)
    '()
    (let ((right-op) (right-to-left-list-of-values (rest-operands exps) env))
      (cons (eval (first-operand exps) env)
            right-op))))
