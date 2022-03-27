#lang racket

(define (append seq1 seq2)
  (cond ((empty? seq1) seq2)
        ((not (pair? seq1)) seq1)
        (else (cons (append (car seq1) seq2)
                    (append (cdr seq1) seq2)))))

(define (equals? a b)
  (cond ((empty? a) #t)
        ((and (not (pair? a)) (not (pair? b))) (eq? a b))
        (else (and (equals? (car a) (car b))
                   (equals? (cdr a) (cdr b))))))
         
;(eq? '(this is a list) '(this is a list))
(equals? '(this (is a) list) '(this (is a) list))
