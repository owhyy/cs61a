#lang racket
;; 2.23
(define (for-each f l)
  (if (empty? l) #t
    (begin (f (car l))
           (for-each f (cdr l)))))
;; ---

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

;; 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))
;; ---

;; frame-coord-map
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                (scale-vect (ycor-vect v) (edge1-frame frame))))))

;; 2.48
(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))
;; ---

(define (draw-line l)
  #t) ;; implementation not yet known

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame)
           (start-segment segment))
          ((frame-coord-map frame)
           (end-segment segment))))
      segment-list)))

;; a
(define outline->painter
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 0 1))
                           (make-segment (make-vect 0 1) (make-vect 1 1))
                           (make-segment (make-vect 1 1) (make-vect 1 0))
                           (make-segment (make-vect 0 0) (make-vect 1 0)))))

;; b
(define X->painter
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                           (make-segment (make-vect 0 1) (make-vect 1 0)))))

;; c
(define diamond->painter
  (segments->painter (list (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                           (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
                           (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
                           (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))))

;; d
;; not my cooridinates
(define wave->painter
  (segments->painter (list
                       (make-segment (make-vect .25 0) (make-vect .35 .5))
                       (make-segment (make-vect .35 .5) (make-vect .3 .6))
                       (make-segment (make-vect .3 .6) (make-vect .15 .4))
                       (make-segment (make-vect .15 .4) (make-vect 0 .65))
                       (make-segment (make-vect 0 .65) (make-vect 0 .85))
                       (make-segment (make-vect 0 .85) (make-vect .15 .6))
                       (make-segment (make-vect .15 .6) (make-vect .3 .65))
                       (make-segment (make-vect .3 .65) (make-vect .4 .65))
                       (make-segment (make-vect .4 .65) (make-vect .35 .85))
                       (make-segment (make-vect .35 .85) (make-vect .4 1))
                       (make-segment (make-vect .4 1) (make-vect .6 1))
                       (make-segment (make-vect .6 1) (make-vect .65 .85))
                       (make-segment (make-vect .65 .85) (make-vect .6 .65))
                       (make-segment (make-vect .6 .65) (make-vect .75 .65))
                       (make-segment (make-vect .75 .65) (make-vect 1 .35))
                       (make-segment (make-vect 1 .35) (make-vect 1 .15))
                       (make-segment (make-vect 1 .15) (make-vect .6 .45))
                       (make-segment (make-vect .6 .45) (make-vect .75 0))
                       (make-segment (make-vect .75 0) (make-vect .6 0))
                       (make-segment (make-vect .6 0) (make-vect .5 .3))
                       (make-segment (make-vect .5 .3) (make-vect .4 0))
                       (make-segment (make-vect .4 0) (make-vect .25 0))
                       )))
