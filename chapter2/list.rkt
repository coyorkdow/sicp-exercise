#lang racket

(provide filter)
(provide flatmap)
(provide enumerate-interval)

(define (map proc seq)
  (if (null? seq)
     seq
     (cons (proc (car seq)) (map proc (cdr seq)))))

(define (last-pair p)
  (cond ((null? p) p)
       ((null? (cdr p)) (car p))
       (else (last-pair (cdr p)))))

(define (reverse p)
  (cond ((null? p) p)
       ((null? (cdr p)) (list (car p)))
       (else (cons (car p) (reverse (cdr p))))))

(define (filter predicate seq)
  (cond ((null? seq) seq)
       ((predicate (car seq)) (cons (car seq) (filter predicate (cdr seq))))
       (else (filter predicate (cdr seq)))))

(define (accumulate op init seq)
  (if (null? seq)
     init
     (op (car seq) (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
     null
     (cons low (enumerate-interval (+ 1 low) high))))
