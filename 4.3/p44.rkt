#lang sicp

(define col-symbols '(a b c d e f g h))
(define row-symbols '(1 2 3 4 5 6 7 8))

(define vect cons)

(define (col q)
  (car q))

(define (row q)
  (cdr q))

(define (same-col qx qy)
  (eq? (col qx) (col qy)))

(define (same-row qx qy)
  (eq? (row qx) (row qy)))

(define diag-vectors
  (list (vect 1 1)
        (vect 1 -1)
        (vect -1 1)
        (vect -1 -1)))

(define (add-vect sq vect)
  (cons (+ (col vect) (col sq))
        (+ (row vect) (row sq))))

(define (oob? sq)
  (or (= (col sq) -1)
      (= (col sq) 8)
      (= (row sq) -1)
      (= (row sq) 8)))

(define (diag-path start vect)
  (let ((next-square (add-vect start vect)))
    (if (oob? next-square)
      '()
      (cons next-square (diag-path next-square vect)))))

(define (diag-paths q)
  (map (lambda (vect)
         (diag-path q vect))
       diag-vectors))

(define (any? predicate items)
  (cond ((null? items) #f)
        ((predicate? (car items)) #f)
        (else (any predicate (cdr items)))))

(define (same-diag? qx qy)
  (any? (lambda (q) (eq? q qy))
        (diag-paths qx)))

(define (amb-range)
  (amb (range 0 8)))

(define (amb-queen x)
  (map cons
       (amb-range)
       (amb-range)))

(define (every? pred items)
  (if (null? items)
    #t
    (and (pred (car items))
         (every? (cdr items)))))

(define (check-queen q rest)
  (if (null? rest)
    #t
    (and
      (not
        (or (any? (map (lambda (x) (same-diag q x))
                       rest))
            (any? (map (lambda (x) (same-col q x))
                       rest))
            (any? (map (lambda (x) (same-row q x))
                       rest))))
      (check-queen (car rest) (cdr rest)))))

(define (solve)
  (let ((queens (map amb-queen (range 0 8))))
    (require (distinct queens))
    (require (every? (map check-queen)))
    queens))

(define (print-queens queens)
  (if (not (null? queens))
    (begin
      (display "(")
      (display (list-ref (col (car queens)) col-symbols))
      (display (list-ref (row (car queens)) row-symbols))
      (display ")")
      (newline)
      (print-queens (cdr queens)))))
