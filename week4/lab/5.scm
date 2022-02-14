#lang racket

(define (last-element-as-list loe)
  (let ((next-loe (if (empty? loe) loe (last-element-as-list (cdr loe)))))
    (if (empty? next-loe) loe
        (last-element-as-list (cdr loe)))))

(define (last-element loe)
  (car (last-element-as-list loe)))

(define (but-last loe)
  (cond ((empty? loe) empty)
        ((equal? (car loe) (last-element loe)) empty)
        (else (cons (car loe)
                    (but-last (cdr loe))))))

(define (reverse loe)
  (cond ((empty? loe) null)
        (else  (cons (last-element loe)
                     (reverse (but-last loe))))))

(reverse (list 1 4 9 16 25))
