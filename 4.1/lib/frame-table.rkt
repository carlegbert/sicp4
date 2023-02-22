#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

;; make an unique object so that we can store
;; null/false in our table & indicate to the caller
;; that we're returning nothing instead of one of those
;; actual values (without exposing the rest of the table's
;; implementation details). There's probably a less kludgy
;; way to do this in racket but I don't know what it is.
(define undefined (lambda () '()))

(define (undefined? val)
  (eq? val undefined))

(define (make-table)
  (let ((local-table (mcons '*table* '())))
    (define (lookup key)
      (let ((result (massoc key table)))
        (if (not result)
          undefined
          (cdr result))))

    (define (insert! key value)
      (let ((result (massoc key table)))
        (if (not result)
          (set-cdr! table
                    (mcons (mcons key value)
                           (mcdr table)))
          (set-cdr! result value))
        'ok))

    (define (update! key value)
      (let ((result (massoc key table)))
        (if (not result)
          #f
          (begin
            (set-cdr! result value)
            #t))))

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'set!) update!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(provide
  make-table
  undefined?)
