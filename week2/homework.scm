#lang racket
(require berkeley)
(define (every f xs)
  (if (empty? xs) empty
      (cons (f (first xs)) (every f (bf xs)))))

(define (keep pred xs)
  (if (empty? xs) empty
      (if (pred (first xs)) (cons (first xs) (keep pred (bf xs)))
            (keep pred (bf xs)))))

(every (lambda (letter) (word letter letter)) 'purple)
(every (lambda (number) (if (even? number) (word number number) number))
       '(781 5 76 909 24))
(keep even? '(781 5 76 909 24))
(keep (lambda (letter) (member? letter 'aeiou)) 'bookkeeper)
(keep (lambda (letter) (member? letter 'aeiou)) 'syzgyzgy)
;(keep (lambda (letter) (member? letter 'aeiou)) '(purple syzgyzgy))
(keep (lambda (wd) (member? 'e wd)) '(purple syzgyzgy))


;; extra
;; basically, we need a way to make our factorial be recursive lambda function
;; 1. we can do this by composition of functions (f f). This is what the first part does:
;; calls f(f(x)) (the same way combining f and g calls f(g(x))
;; 2. the part that actually does the work (n) is wrapped inside r, because r is actually the
;; recursive part, which calls itself untill n is 0 
(((lambda (f) (f f)) (lambda (r) (lambda (n) (if (= n 0) 1 (* n ((r r) (- n 1))))))) 5)