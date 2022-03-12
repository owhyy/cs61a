#lang racket

(define x (list (list 1 2) (list 3 4)))
(define (fringe t)
  (cond ((empty? t) empty)
        ((not (pair? t)) (list t))
        (else (append (fringe (car t))
                      (fringe (cdr t))))))

(define y (list x x))
y
(fringe y)
