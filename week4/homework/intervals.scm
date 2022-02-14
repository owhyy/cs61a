#lang racket

(define (make-interval a b)
  (cons a b))

(define (lower-bound i)
  (car i))

(define (upper-bound i)
  (cdr i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y)))) 

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
        
(define (div-interval x y)
  (if (or (and (<= 0 (upper-bound y))
               (>= 0 (lower-bound y)))
          (and (<= 0 (upper-bound x))
               (>= 0 (lower-bound x))))
      (error "ERROR: Interval cannot span 0!")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(define (make-center-percent c p)
  (make-interval (- c (* p c))
                 (+ c (* p c))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (/ (- (upper-bound i) (lower-bound i))
     (* 2 (center i))))

(define test-i (make-center-percent 6.8 0.8))
(percent test-i)