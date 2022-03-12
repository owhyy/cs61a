#lang racket

(define (square x) (* x x))

(define (accumulate f base sequence)
  (cond ((empty? sequence) base)
        (else (f (car sequence) 
                 (accumulate f base (cdr sequence))))))

;; map
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) empty sequence))
(map square (list 1 2 3 4))

;; append

;(define (append seq1 seq2)
;  (define (loop s acc)
;    (cond ((empty? s) acc)
;          ((not (pair? s)) s)
;          (else (loop (cdr s) (cons (car s) acc)))))
;  (loop (list seq1 seq2) empty))
;(define (append seq1 seq2)
;  (accumulate cons (map cdr (list seq1 seq2)) empty ))

;(append (list 1 2) (list 3 4))

;; length
(define (length sequence)
  (accumulate
   +
   0 
   (map (lambda (x) (if (not (empty? x)) 1 (length x))) sequence)))