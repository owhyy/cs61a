# Week 4 Lab

1.
```scheme
(define x (cons 4 5))
(car x) ; 4
(cdr x) ; 5
(define y (cons 'hello 'goodbye))
(define z (cons x y)) ; ((4 5) ('hello 'goodbye))
(car (cdr z)) ; 'hello
(cdr (cdr z)) ; 'goodbye
```
&nbsp;

2.
```scheme
(cdr (car z)) ; 5
(car (cons 8 3)) ; 8
(car z) ; (4 5)
(car 3) ; '() wrong: error. makes sense
```
&nbsp;

3, 4 and 5.
```scheme
(define (make-rational num den)
  (cond ((or (and (> num 0) (> den 0)) (and (< num 0) (< den 0)))
         (cons (abs num) (abs den)))
        (else (cons num den))))

(define (numerator rat)
  (car rat))
(define (denominator rat)
  (cdr rat))
(define (*rat a b)
  (make-rational (* (numerator a) (numerator b))
                 (* (denominator a) (denominator b))))
(define (print-rat rat)
  (word (numerator rat) '/ (denominator rat)))
(define (+rat a b)
  (make-rational (+ (* (numerator a) (denominator b))
                    (* (numerator b) (denominator a)))
                 (* (denominator a) (denominator b))))
```
&nbsp;

6.

### Excersise 2.2 and 2.3
```scheme
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
```
&nbsp;

### Excersise 2.4
```scheme
(define (cons x y)
  (lambda (m) (m x y)))
```

The lambda acts as the binding agent; It creates a procedure that applies `m` to `x` and `y`, although, in reality, it doesn't apply anything

```scheme
(define (car z)
  (z (lambda (p q) p)))
```

Here `z` is the `cons` function, which will get applied to `(lambda (p q) p)`\
Something like `(cons 3 5)` will result in `((cons 3 5) (lambda (p q) p))`\
`(cons 3 5)` will get applied to the lambda taking exactly 2 arguments (3 as p, 5 as q)
and the lambda will return `p` -> 3
```scheme
(define (cdr z)
  (z (lambda (p q) q)))
```

Same thing, except it will return `q` -> 5

&nbsp;

7.
```scheme
(define x '(a (b c) d))
(car x) ;; 'a is the first element of the sentence (list) -> its what get returned by car
(cdr x) ;; '((b c) d) gets represented as a sentence
(car (cdr (cdr x)))
(car (cdr x))
```
&nbsp;

8.
```scheme
(define (last-element-as-list loe)
  (let ((next-loe (if (empty? loe) loe (last-element-as-list (cdr loe)))))
    (if (empty? next-loe) loe
        (last-element-as-list (cdr loe)))))

(define (last-element loe)
  (car (last-element-as-list loe)))

(define (but-last loe)
  (cond ((empty? loe) empty)
        ((equal? (car loe) (last-element loe)) empty)
        (else (cons (car loe)
                    (but-last (cdr loe))))))

(define (reverse loe)
  (cond ((empty? loe) null)
        (else  (cons (last-element loe)
                     (reverse (but-last loe))))))
```
Basically `reverse` will `cons` the last element of the list to the everything but the last (`butlast`) of it.
