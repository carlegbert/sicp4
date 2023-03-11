#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(#%require "./util.rkt")
(#%require "./environment.rkt")
(#%require "./primitives.rkt")
(#%require "./procedure.rkt")

(define (actual-value expr env)
  (force-it (lazy-eval expr env)))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (delay-it expr env)
  (mlist 'thunk expr env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))


(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (lazy-eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (lazy-eval (first-expr exps) env))
        (else (lazy-eval (first-expr exps) env)
              (eval-sequence (rest-exps exps env)))))

(define (eval-assignment expr env)
  (set-variable-value! (assignment-variable expr)
                       (lazy-eval (assignment-value expr) env)
                       env)
  'ok)

(define (eval-definition expr env)
  (define-variable! (definition-variable expr)
                    (lazy-eval (definition-value expr) env)
                    env)
  'ok)

(define (self-evaluating? expr)
  (cond ((number? expr) #t)
        ((string? expr) #t)
        (else #f)))

(define (variable? expr) (symbol? expr))

(define (quoted? expr)
  (tagged-list? expr 'quote))

(define (assignment? expr)
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

(define (if? expr)
  (tagged-list? 'if expr))

(define (eval-if expr env)
  (if (true? (actual-value (if-predicate expr) env))
    (lazy-eval (if-consequent expr) env)
    (lazy-eval (if-alternative expr) env)))

(define (list-of-arg-values expr env)
  (if (no-operands? expr)
    '()
    (cons (actual-value (first-operand expr) env)
          (list-of-arg-values (rest-operands expr) env))))

(define (list-of-delayed-args expr env)
  (if (no-operands? expr)
    '()
    (cons (delay-it (first-operand expr) env)
          (list-of-delayed-args (rest-operands expr) env))))


(define (applyl procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
           procedure
           (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             (list-of-delayed-args arguments env)
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))

(define (text-of-quotation expr)
  expr)

(define (lazy-eval expr env)
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
         (lazy-eval (cond->if expr) env))
        ((application? expr)
         (applyl (actual-value (operator expr) env) (operands expr) env))
        (else
          (error "Unknown expression type -- EVAL" expr))))

(provide
  lazy-eval
  actual-value)
