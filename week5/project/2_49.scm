#lang racket

;; 2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect   
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
  (make-vect      
   (* (xcor-vect v) s)
   (* (ycor-vect v) s)))
;; ---

;; 2.48
(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))
;; ---

;(define (draw-line l)
;  ()) ;; implementation not yet known

(define (for-each f l)(
  (if (not (null? l)) ((f (car l)) null)
      (for-each f (cdr l))))

;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map fram)
;         (start-segment segment))
;        ((frame-coord-map frame)
;         (end-segment segment))))
;     segment-list)))


 