#lang racket
; Segment implementation
; ============================
(define (make-point x y)
  (cons x y))
(define (get-x point)
  (car point))
(define (get-y point)
  (cdr point))
(define (make-segment p1 p2)
  (cons p1 p2))
(define (starting-point segment)
  (car segment))
(define (ending-point segment)
  (cdr segment))
(define (midpoint-segment line)
  (make-point (/ (+ (get-x (starting-point line))
                    (get-x (ending-point line)))
                 2)
              (/ (+ (get-y (starting-point line))
                    (get-y (ending-point line)))
                 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (get-x p))
  (display ", ")
  (display (get-y p))
  (display ")"))

;(print-point (midpoint-segment (make-segment (make-point 4 4) (make-point 6 10))))

; ============================
; top-left and bottom-right are points
; to be called with (make-rectangle (make-point 0 0) (make-point 200 100))

(define (square x)
  (* x x))

; overkill??
(define (distance p1 p2)
  (sqrt (+ (square (- (get-x p2) (get-x p1)))
           (square (- (get-y p2) (get-y p1))))))

; First rectangle implementation: top-left point and bottom right point
; ============================
(define (make-rectangle top-left bottom-right)
  (cons top-left bottom-right))

(define (top-left rectangle)
  (car rectangle))

(define (top-right rectangle)
  (cons (get-x (bottom-right rectangle))
        (get-y (top-left rectangle))))

(define (bottom-left rectangle)
  (cons (get-x (top-left rectangle))
        (get-y (bottom-right rectangle))))

(define (bottom-right rectangle)
  (cdr rectangle))

(define (height rectangle)
  (distance (top-left rectangle) (bottom-left rectangle)))

(define (width rectangle)
  (distance (top-left rectangle) (top-right rectangle)))

(define (get-area rectangle)
  (* (width rectangle) (height rectangle)))

(define (get-perimeter rectangle)
  (* (+ (width rectangle) (height rectangle)) 2))

; ============================
; implementation 2: top-left with height and width
; should this work the same way as impl. 1? in the s
(define (make-rectangle-alt top-left width height)
  (cons top-left
        (cons width height)))

(define (width-alt rectangle)
  (car (cdr rectangle)))

(define (height-alt rectangle)
  (cdr (cdr rectangle)))

(define (top-left-alt rectangle)
  (car rectangle))
(define (top-right-alt rectangle)
  (make-point (+ (get-x (top-left-alt rectangle)) (width-alt rectangle))
              (get-y (top-left-alt rectangle))))
(define (bottom-left-alt rectangle)
  (make-point (get-x (top-left-alt rectangle))
              (+ (get-y (top-left-alt rectangle)) (height-alt rectangle))))
(define (bottom-right-alt rectangle)
  (make-point (get-x (top-right-alt rectangle))
              (get-y (bottom-left-alt rectangle))))

(define r1 (make-rectangle-alt (make-point 0 0) 5 10))
(define r2 (make-rectangle (make-point 0 0) (make-point 5 10)))
(width-alt r1)
(height-alt r1)
(top-left-alt r1)
(top-right-alt r1)
(bottom-left-alt r1)
(bottom-right-alt r1)

(get-area r1)
(get-perimeter r1)
(get-area r2)
(get-perimeter r2)
