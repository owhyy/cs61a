#lang racket

(define (even? x)
  (= (remainder x 2) 0))

(define (same-parity f . r)
  (define (handle-even r)
    (cond ((null? r) null) 
          ((even? (car r)) (cons (car r) (handle-even (cdr r))))
          (else (handle-even (cdr r)))))
  (define (handle-odd r)
    (cond ((null? r) null) 
          ((not (even? (car r))) (cons (car r) (handle-odd (cdr r))))
          (else (handle-odd (cdr r)))))
  
  (cons f (if (even? f)
              (handle-even r)
              (handle-odd r))))
(define (test f . r)
  (if (even? f) 10 (if (null? r) null (test (cdr r)))))

;(test 1 40)

(same-parity 1 2 3 4 5 6)