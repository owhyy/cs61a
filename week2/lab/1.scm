#lang racket
(require berkeley)

;1
(lambda (x) (+ x 3))
((lambda (x) (+ x 3)) 7)

(define (make-adder num)
  (lambda (x) (+ x num)))
((make-adder 3) 7)

(define plus3 (make-adder 3))
(plus3 7)

(define (square x) (* x x))
(square 5)
(define sq (lambda (x) (* x x)))
(sq 5)
(define (try f) (f 3 5))
(try +)
(try word)

;2
(define (substitute sent old new)
  (cond ((empty? sent) empty)
        ((equal? (first sent) old)
         (se new (substitute (bf sent) old new)))
        (else (se (first sent) (substitute (bf sent) old new)))))
(substitute '(she loves you yeah yeah yeah) 'yeah 'maybe)

;3
(define (g) (lambda (x) (+ x 2)))
((g) 1)
;; g is a function that takes 0 arguments and returns a #procedure<...>

;4
(define (f) (lambda (x) (lambda (y) y) (lambda (z) z)))
;f -> any procedure, taking any number of arguments
;(f) -> any procedure taking 0 arguments
;(f 3) -> a procedure taking exactly one argument
;((f)) -> a procedure taking 0 arguments and returning a procedure taking 0 arguments, such as + or *
;(((f)) 3) -> nothing works

;5
(define (t f)
  (lambda (x) (f (f (f x)))))
(define (1+ x) (+ 1 x))

;6
(define (s x)
  (+ 1 x))

;7
(define (make-tester w)
  (lambda (x) (equal? x w)))

((make-tester 'hal) 'hal)
((make-tester 'hal) 'cs61a)
(define sicp-author-and-astronomer? (make-tester 'gerry))
(sicp-author-and-astronomer? 'hal)
(sicp-author-and-astronomer? 'gerry)