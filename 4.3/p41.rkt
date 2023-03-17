#lang sicp

(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

(define (check-combo dwellers)
  (let ((baker (car dwellers))
        (cooper (cadr dwellers))
        (fletcher (caddr dwellers))
        (miller (cadddr dwellers))
        (smith (car (cddddr dwellers))))
    (cond
      ((not (distinct? (list baker cooper fletcher miller smith))) #f)
      ((= baker 5) #f)
      ((= cooper 1) #f)
      ((= fletcher 5) #f)
      ((= fletcher 1) #f)
      ((< miller cooper) #f)
      ((= 1 (abs (- smith fletcher))) #f)
      ((= 1 (abs (- cooper fletcher))) #f)
      (else #t))))

(define (advance items)
  (if (= (car items) 5)
    (cons 1 (advance (cdr items)))
    (cons (+ 1 (car items)) (cdr items))))

(define (solve dwellers)
  (if (check-combo dwellers)
    dwellers
    (solve (advance dwellers))))

(solve (list 1 1 1 1 1))
