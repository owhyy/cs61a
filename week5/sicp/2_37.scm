#lang racket
(require rackunit)

(define (accumulate p init seq)
  (if (empty? seq)
    init
    (p (car seq)
       (accumulate p init (cdr seq)))))

(define (accumulate-n p init seqs)
  (if (empty? (car seqs))
    null
    (cons (accumulate p init (map car seqs))
          (accumulate-n p init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w)
         (dot-product w v)) m))

(define (transpose mat)
  (accumulate-n cons empty mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define matrix '((1 2 3 4) (4 5 6 6)))
(define tests
  (test-suite
    "Tests for excersie 2.37"

    ; accumulate test
    (check-equal? (accumulate + 0 '(1 2 3 4)) 10 "accumulate test")
    ; accumulate-n test
    (check-equal? (accumulate-n + 0 '((1 2 3 4) (0 0 0 0))) '(1 2 3 4) "accumulate-n test")
    ; dot-product test
    (check-equal? (dot-product (car matrix) (car matrix)) 30 "dot-product test")
    ; matrix-*-vector test
    (check-equal? (matrix-*-vector matrix '(1 1 1 1)) '(10 21) "matrix-*-vector test")
    ; transpose test
    (check-equal? (transpose matrix) '((1 4) (2 5) (3 6) (4 6)) "transpose test")
    ; matrix-*-matrix test
    (check-equal? (matrix-*-matrix matrix '((1 1) (1 1) (1 1) (1 1))) '((10 10) (21 21)) "matrix-*-matrix test")
    ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
