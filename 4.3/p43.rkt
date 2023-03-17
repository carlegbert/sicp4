#lang sicp

(define (family father daughter boat)
  (list father daughter boat))

(define (father family)
  (car family))

(define (daughter family)
  (cadr family))

(define (boat family)
  (caddr family))

(define daughter-names
  (list 'mary
        'gabrielle
        'lorna
        'rosalind
        'melissa))

(define father-names
  (list
    'moore
    'downing
    'hall
    'barnacle
    'parker))

(define (solve)
  ;; the obvious constraints filled in manually here;
  ;; the correct spirit would probably involve everything
  ;; being amb'd.
  (let* ((barnacle
          (family 'barnacle (amb daughter-names) 'gabrielle))
        (moore
          (family 'moore 'mary 'lorna))
        (hall
          (family 'hall (amb daughter-names) 'rosalind))
        (downing
          (family 'downing (amb daughter-names) 'melissa))
        (parker
          (family 'parker (amb daughter-names) (amb daughter-names)))
        (all-families
          (list barnacle moore hall downing parker)))
    (require (eq? (daughter parker) (barnacle boat)))
    (require (distinct (map daughter all-families)))
    (require (distinct (map boat all-families)))
    (let ((lornas-family (amb all-families)))
      (require (eq? (daughter lornas-family 'lorna)))
      (father lornas-family))))
