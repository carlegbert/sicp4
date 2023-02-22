#lang racket

(require rnrs/mutable-pairs-6)

(#%require "./environment.rkt")

(define (evaln expr env)
  (cond ((self-evaluating? expr) expr)
        ((variable? expr) (lookup-variable-value expr env))
        ((quoted? expr) (text-of-quotation expr))
        ((assignment? expr) (eval-assignment expr env))
        ((definition? expr) (eval-definition expr env))
        ((if? expr) (eval-if expr env))
        ((lambda? expr)
         (make-procedure (lambda-parameters expr)
                         (lambda-body expr)
                         env))
        ((begin? expr)
         (eval-sequence (begin-actions expr) env))
        ((cond? expr)
         (evaln (cond->if expr) env))
        ((application? expr)
         (applyn (evaln (operator expr) env)
                (list-of-values (operands expr) env)))
        (else
          (error "Unknown expression type -- EVAL" expr))))

(define (applyn procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (evaln (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval-if expr env)
  (if (true? (evaln (if-predicate expr) env))
    (evaln (if-consequent expr) env)
    (evaln (if-alternative expr) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (evaln (first-expr exps) env))
        (else (evaln (first-expr exps) env)
              (eval-sequence (rest-exps exps env)))))

(define (eval-assignment expr env)
  (set-variable-value! (assignment-variable expr)
                       (evaln (assignment-value expr) env)
                       env)
  'ok)

(define (eval-definition expr env)
  (define-variable! (definition-variable expr)
                    (evaln (definition-value expr) env)
                    env)
  'ok)

(define (self-evaluating? expr)
  (cond ((number? expr) #t)
        ((string? expr) #t)
        (else #f)))

(define (variable? expr) (symbol? expr))

(define (tagged-list? expr tag)
  (if (pair? expr)
    (eq? (car expr) tag)
    #f))

(define (quoted? expr)
  (tagged-list? expr 'quote))

(define (assignment? ex)
  (tagged-list? expr 'set!))

(define (assignment-variable expr) (cadr expr))

(define (assignment-value expr) (caddr expr))

(define (definition? expr)
  (tagged-list? expr 'define))

(define (definition-variable expr)
  (if (symbol? (cadr expr))
    (cadr expr)
    (caadr expr)))

(define (definition-value expr)
  (if (symbol? (cadr expr))
    (caddr expr)
    (make-lambda (cdadr expr)
                 (cddr expr))))

(define (lambda? expr) (tagged-list? expr 'lambda))

(define (lambda-parameters expr) (cadr expr))

(define (lambda-body expr) (cddr expr))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? expr)
  (tagged-list? expr 'if))

(define (if-predicate expr)
  (cadr expr))

(define (if-consequent expr)
  (caddr expr))

(define (if-alternative expr)
  (if (not (null? (cdddr expr)))
    (cadddr expr)
    #f))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? expr)
  (tagged-list? expr 'begin))

(define (begin-actions expr) (cdr expr))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-expr seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (make-begin seq) (cons 'begin seq))

(define (sequence->expr seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-expr seq))
        (else (make-begin seq))))

(define (application? expr) (pair? expr))

(define (operator expr) (car expr))

(define (operands expr) (cdr expr))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (cond? expr) (tagged-list? expr 'cond))

(define (cond-clauses expr) (cdr expr))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if expr)
  (expand-clauses (cond-clauses expr)))

(define (expand-clauses clauses)
  (if (null? clauses)
    #f
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->expr (cond-actions first))
          (error "ELSE clause isn't last -- COND->IF" clauses))
        (make-if (cond-predicate first)
                 (sequence->expr (cond-actions first))
                 (expand-clauses rest))))))

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? 'procedure p))

(define (procedure-parameters p)
  (cadr p))

(define (procedure-body p)
  (caddr p))

(define (procedure-environment p)
  (cadddr p))

(provide
  make-lambda
  sequence->expr
  make-begin)

