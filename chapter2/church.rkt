#lang racket

; The five Peano axioms are the following:[35][i]
; 1. Zero is a natural number.
; 2. Every natural number has a successor which is also a natural number.
; 3. Zero is not the successor of any natural number.
; 4. If the successor of @x equals the successor of @y, then @x equals @y.
; 5. The axiom of induction: If a statement is true of 0, and if the truth of that statement for a
;    number implies its truth for the successor of that number, then the statement is true for every
;    natural number.

; Church numerials are kind of the abstraction of the nature numbers. They always take two parameters,
; @f and @x. @f represents an procedure to get the successor of an entity (i.e. @f: n -> successor n),
; and @x represents an entity, "zero". Church numerials don't represent any concrete meanings, They
; are a set of procedures to perform "successor" operation and "zero". The \n-th numeric is to perform 
; the given "successor" operation \n times on the given entity "zero". We can define any kind of the
; "successor" and the "zero". For example, "successor" can be defined as to add a pair of parentheses
; contains within the current entity, while "zero" is defined as literal "nil". Hence, we have nil,
; (nil), ((nil)), etc,.

; No matter what "successor" is, "zero" is "zero".
(define zero (lambda (f) (lambda (x) x)))

; @n is a certain church numeric. For any given @f and @x, after @n took @f and @x, let it be the
; argument which taken by @f. Thus, if @n performs @f on @x \n times, then it will perform exactly
; \n+1 times.
(define (successor n) (lambda (f) (lambda (x) (f ((n f) x)))))

; It's equivalent to (successor zero)
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x))))) ; ditto

; Both @a and @b are church numerics. @a plus @b is equivalent to perform @successor \a times on @b.
; This definition is equivalent to
; (define (add a b) ((a successor) b))
(define (add a b) (lambda (f) (lambda (x) ((a f) ((b f) x)))))

; Both @a and @b are church numerics. @a times @b is equivalent to take performing @successor \b times
; as a procedure, preform this procedure \a times on zero.
; This definition is equivalent to
; (define (mul a b) ((a (b successor)) zero))
(define (mul a b) (lambda (f) (lambda (x) ((a (b f)) x))))

; Both @a and @b are church numerics. @a power @b is equilvalent to perform @a \b times. @a performs a
; certain procedure \a times. Performing @a once is just perform @a. But when performing @a twice, the
; procedure that perform a certain procedure \a times will be performed \a times, that is, \a*\a times
; in total.
(define (exp a b) (b a))

; ====================testing====================

(define (f x) (string-append "(" x ")"))
(define x "nil")

(define three (successor two))
(define four (add two two))
(define five (add two three))
(define six (mul two three))
(define seven (add five two))
(define eight (exp two three))
(define nine (exp three two))

((zero f) x)  ; nil
((one f) x)   ; (nil)
((two f) x)   ; ((nil))
((three f) x) ; (((nil)))
((four f) x)  ; ((((nil))))
((five f) x)  ; (((((nil)))))
((six f) x)   ; ((((((nil))))))
((seven f) x) ; (((((((nil)))))))
((eight f) x) ; ((((((((nil))))))))
((nine f) x)  ; (((((((((nil)))))))))