#lang racket

;; 2_46
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

;; 2_47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))
;; ---

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (painter (make-frame
                 new-origin
                 (sub-vect (m corner1) new-origin)
                 (sub-vect (m corner2) new-origin))))))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-up
            (transform-painter
              painter1
              (make-vect 1 1)
              split-point
              (make-vect 1 0)))
          (paint-down
            (transform-painter
              painter2
              split-point
              (make-vect 0 1)
              (make-vect 1 0.5))))
      (lambda (frame)
        (paint-up frame)
        (paint-down frame)))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
            (transform-painter
              painter1
              (make-vect 0 0)
              split-point
              (make-vect 0 1)))
          (paint-right
            (transform-painter
              painter2
              split-point
              (make-vect 1 0)
              (make-vect 0.5 1))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
