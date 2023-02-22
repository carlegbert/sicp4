#lang racket

(define (eval-cond expr env)
  (eval-cond-clauses (cond-clauses expr) env))

(define (cond-recipient-clause? clause)
  (tagged-list? (cond-actions clause '=>)))

(define (cond-recipient clause)
  (cadr (cond-actions clause)))

(define (eval-cond-clauses clauses env)
  (if (null? clauses)
    ;; return nothing. (surely there's a better way to do this...)
    (cond)
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (eval (sequence->exp (cond-actions first)) env)
          (error "ELSE clause isn't last -- EVAL-COND" clauses))
        ;; evaluate the predicate only once.
        (let ((evaluated-predicate
                (eval (cond-predicate first) env)))
          (if (true? evaluated-predicate)
            (if (cond-recipient-clause? first)
              (eval (cons (cond-recipient first) (list evaluated-predicate)) env)
              (eval (sequence->exp (cond-actions first)) env))
            (eval-cond-clauses rest env)))))))
