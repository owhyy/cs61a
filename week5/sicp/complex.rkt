#lang racket

(define (square x) (* x x))

;; we can represent complex numbers in two ways:
;; 1. rectangle form: x + yi (real and imag)
;; 2. polar form: angle and magnitude (trigonometric way)

;; for both, the real, imag, magnitude and angle have to return the same thing. however, when we use the x+yi form, we need to calculate magnitude and angle, while when in polar form, we calculate x and y;
;; we can also use both, at the same time, but we'll need some way to distinguish between complex numbers being represented in polar or in square form (if we have the pair (3 4), does this represent the number 3 + 4i or the magnitude of 3 and angle of 4? how will we be able to calculate things like magnitude, if we don't know which formula to use?). to differentiate between the two representations we'll use a type tag.

;; this adds a tag to something
(define (attach-tag type-tag contents)
  (cons type-tag contents))

;; this returns the tag of something
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum: TYPE-TAG" datum)))
;; this returns the actual contents, without the tag
(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum: CONTENTS" datum)))

;; we can now define different procedures for the different representations:
;; RECTANGULAR
;; checking if something is using the rectangular representation
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

;; creating complex numbers using the rectangular representation
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
;; creating a rectangular representation from a rectangular form
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
;; creating a rectangular representation from a magnitude-angle form
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

;; POLAR
;; checking if the number uses the polar representation
(define (polar? z)
  (eq? (type-tag z) 'polar))

;; creating numbers using the polar representation
(define (real-part-polar z) (* (cos (angle-polar z)) (magnitude-polar z)))
(define (imag-part-polar z) (* (sin (angle-polar z)) (magnitude-polar z)))
(define (magnitude-polar z) (car z))
(define (angle-polar     z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-ang-polar r a) (attach-tag 'polar (cons r a)))

;; this makes our implemenetation more GENERIC: we can use 2 different representation, and mean the same thing. we can now implemenet real-part, imag-part, magnitude and angle to work correctly on both, polar and rectangular representation:
(define (real-part z)
  (cond ((polar? z) (real-part-polar (contents z)))
        ((rectangular? z) (real-part-rectangular (contents z)))
        (else (error "Unknown type: REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z) (imag-part-rectangular (contents z)))
        ((polar? z) (imag-part-polar (contents z)))
        (else (error "Unknown type: IMAG-PART" z))))

(define (magnitude z)
  (cond ((polar? z) (magnitude-polar (contents z)))
        ((rectangular? z) (magnitude-rectangular (contents z)))
        (else (error "Unknown type: MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z) (angle-rectangular (contents z)))
        ((polar? z) (angle-polar (contents z)))
        (else (error "Unknown type: ANGLE" z))))

;; now, we only need a way to make our constructors generic too. we can, of course, construct a complex number in rectangular form when we get magnitude and angle, and vice versa, but most naturally will be if we construct


;; by data abstractions, add-complex, sub-complex and all the other operations using complex numbers should work the same, no matter which representation we use:
;; make-from-real-imag and make-from-mag-ang have to return the same thing

;; because our representation is generic, arithmetic it's going to work correctly with any constructor we want;
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (* (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


