#lang racket
(require berkeley)

(define x (cons 4 5))
(car x) ; 4
(cdr x) ; 5
(define y (cons 'hello 'goodbye))
(define z (cons x y)) ; ((4 5) ('hello 'goodbye))
(car (cdr z)) ; 'hello
(cdr (cdr z)) ; 'goodbye

(cdr (car z)) ; 5
(car (cons 8 3)) ; 8
(car z) ; (4 5)
; (car 3) ; '() wrong: error. makes sense

(define (make-rational num den)
  (cond ((or (and (> num 0) (> den 0)) (and (< num 0) (< den 0)))
         (cons (abs num) (abs den)))
        (else (cons num den))))

(define (numerator rat)
  (car rat))
(define (denominator rat)
  (cdr rat))
(define (*rat a b)
  (make-rational (* (numerator a) (numerator b))
                 (* (denominator a) (denominator b))))
(define (print-rat rat)
  (word (numerator rat) '/ (denominator rat)))
(define (+rat a b)
  (make-rational (+ (* (numerator a) (denominator b))
                    (* (numerator b) (denominator a)))
                 (* (denominator a) (denominator b))))

(print-rat (make-rational 2 3))
(print-rat (make-rational -2 3))
(print-rat (*rat (make-rational 2 3) (make-rational 1 4)))
(print-rat (+rat (make-rational 2 3) (make-rational 1 4)))