#lang racket

; f(x) = x - g(x)/Dg(x)

(define dx 0.000001)
(define tol 0.000001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x))
                 dx)))

(define (fixed-point f guess)
  (define (close-enough v1 v2) (< (abs (- v1 v2)) tol))
  (define (try guess_)
    (let ((next (f guess_)))
         (cond ((close-enough next guess_) next)
               (else (try next)))))
  (try guess))


(define (newton-method g guess)
  (define (newton-transform g)
    (lambda (x) (- x 
                   (/ (g x) ((deriv g) x)))))
  (fixed-point (newton-transform g) guess))

(define (square x) (* x x))

; y^2 - x = 0
(define (sqrt x)
  (newton-method (lambda (y) (- (square y) x)) 1.0))

(sqrt 8)