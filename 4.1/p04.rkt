(define (and? exp)
  (tagged-list? exp 'and))

(define (or? exp)
  (tagged-list? exp 'or))

;; special form version
(define (eval-and-predicates predicates env)
    (cond ((null? predicates) #t)
          ((false? (eval (car predicates) env)) #f
          (else
            (eval-and-predicates (cdr predicates env)))))

(define (eval-and exp env)
  (eval-and-predicates (operands exp) env))

(define (eval-or-predicates predicates env)
    (cond ((null? predicates) #f)
          ((true (eval (car predicates) env)) #t)
          (else
            (eval-or-predicates (cdr predicates env)))))

(define (eval-or exp env)
  (eval-or-predicates (operands exp) env))

;; derived expression version
(define (and->if exp)
  (expand-and-predicates (and-predicates exp)))

(define (predicates exp)
  (cdr exp))

(define (expand-and-predicates predicates)
  (if (null? predicates)
    #t
    (make-if (car predicates)
             (expand-and-predicates (cdr predicates))
             #f)))

(define (and->or exp)
  (expand-or-predicates (or-predicates exp)))

(define (expand-or-predicates predicates)
  (if (null? predicates)
    #f
    (make-if (car predicates)
             #t
             (expand-or-predicates (cdr predicates)))))
