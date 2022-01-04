#lang racket
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
;(define (+ a b)
;  (if (= a 0) b (inc (+ (dec a ) b))))

;(+ 4 5)
; (if (= 4 0) 5 (inc (+ (dec 4) 5))
; (inc (+ (dec 4) 5)
; (+ 1 (+ (- 4 1) 5)
; (+ 1 (+ 3 5))
; (+ 1 (if (= 3 0) 5 (inc (+ (dec 3) 5))
; (+ 1 (+ 1 (+ 2 5))
; (+ 1 (+ 1 (+ 1 (+ 1 5))
; (+ 1 (+ 1 (+ 1 (+ 1 (+ 0 5))
; (+ 1 (+ 1 (+ 1 (+ 1 5))
; (+ 1 (+ 1 (+ 1 6))
; (+ 1 (+ 1 7))
; (+ 1 8)
; 9
; this is a linear recursive process


;(define (+ a b)
;  (if (= a 0) b (+ (dec a) (inc b))))
; (+ 4 5)
; (if (= 4 0) 5 (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9
; this is a linear iterative process