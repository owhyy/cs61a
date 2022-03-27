#lang racket

(define (accumulate p init seq)
  (if (empty? seq)
      init
      (p (car seq)
         (accumulate p init (cdr seq)))))

(define (accumulate-n p init seqs)
  (if (empty? (car seqs))
      init
      (cons (accumulate p init (map car seqs))
            (accumulate-n p init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (map (lambda (y) (* v y)) x)) m))

(define (transpose mat)
  (accumulate-n cons empty mat))

;;  ???
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map m)))
(define v1 '((1 2 3 4) (4 5 6 6)))

; 1*1 + 2*2 + 3*3 + 4*4 = 30
(dot-product (car v1) (car v1))

; new matrix, everything is multiplied by a vector
(matrix-*-vector v1 3)
(transpose v1)
(matrix-*-matrix v1 v1)