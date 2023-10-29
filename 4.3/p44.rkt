#lang sicp

(define (require condition)
  (if (not condition)
    (amb)))

(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

(define col-symbols '(a b c d e f g h))
(define row-symbols '(1 2 3 4 5 6 7 8))

(define (col q)
  (car q))

(define (row q)
  (cdr q))

(define (amb-range)
  (amb 0 1 2 3 4 5 6 7))

(define (amb-queen col)
  (cons col (amb-range)))

(define (up-diag queen)
  (- (row queen) (col queen)))

(define (down-diag queen)
  (+ (row queen) (col queen)))

(define (solve)
  (define (iter queens)
    ;; We get the distinct column for free thanks
    ;; to only creating new queens on unoccupied
    ;; columns.
    ; (require (distinct? (map col queens)))

    ;; No queen may share a row, of course.
    (require (distinct? (map row queens)))

    (require (distinct? (map up-diag queens)))
    (require (distinct? (map down-diag queens)))

    (if (= (length queens) 8)
      queens
      (iter (cons (amb-queen (length queens)) queens))))
  (iter '()))

(define (print-queens queens)
  (if (not (null? queens))
    (begin
      (display "(")
      (display (list-ref col-symbols (col (car queens))))
      (display (list-ref row-symbols (row (car queens))))
      (display ") ")
      (print-queens (cdr queens)))))

(print-queens (solve))
(newline)

; --Q-----
; -----Q--
; ---Q----
; -Q------
; -------Q
; ----Q---
; ------Q-
; Q-------
