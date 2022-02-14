#lang racket

;; doing it like this reverses it
(define (for-eachq f loe)
  (cond ((empty? loe) empty)
        (else (for-each f (cdr loe))
              (f (car loe)))))

;; doing it like this adds the empty to the result
(define (for-each f loe)
  (cond ((empty? loe) #t)
        (else (and (f (car loe))
                   (for-each f (cdr loe))))))
               