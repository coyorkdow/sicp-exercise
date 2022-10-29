#lang racket
(require "list.rkt")

(define empty-board (list))

(define (adjoin-position new-row rest-of-queens) (append (list new-row) rest-of-queens))

(define (safe? positions)
  (define (check rest-pos)
    (if (null? rest-pos)
       #t
       (and (not (= (car positions) (car rest-pos)))
           (check (cdr rest-pos)))))
  (define (gen-cotercorner-helper n rest-pos)
    (define (create-left) (- (car rest-pos) n))
    (define (create-right) (+ (car rest-pos) n))
    (if (null? rest-pos)
       null
       (append (list (create-left) (create-right))
              (gen-cotercorner-helper (+ 1 n) (cdr rest-pos)))))
  (and (check (cdr positions))
      (check (gen-cotercorner-helper 1 (cdr positions)))))


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
       (list empty-board)
       (filter
        safe?
        (flatmap
         (lambda (rest-of-queens)
           (map (lambda (new-row)
                  (adjoin-position new-row rest-of-queens))
               (enumerate-interval 1 board-size)))
         (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)