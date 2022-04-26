;; data-directed programming version of complex number representation

;; the problems with comlex_v1.rkt are that:
;; 1. all of the generic methods must be know and handle differently the different implementations (real-part has to have real-part-rectangular and rea-part-polar to be able to be used with both); now, if we wanted to add a new implementation, we'd have to create new methods (new real-part-...s, and so on) for every of the new representation, and also add new clauses to our generic real-part to use this new real-part-...
;; 2. all the functions (real-part-rectangular, real-part-polar ...) are in the global scope. it's really a pain making sure that no 2 methods have the same name when there's hundreds of different representations

;; the solution is modularizing: each type has a set of operations
;; kinda like a class, that has a bunch of methods
;; and other stuff, except it's called a package. each type has its own operations, and the generic operations will look up the combination of the specific type and apply the correct procedure; this ensures that when we add a new package, we don't need to change the existing procedure (no need to add the new clause in the generic)

;; for this, we'll need two prodeures:
;; (put op type item) adds the item (the representation as a whole) to the generic thing ???, along with its operations and type
;; (get op type) looks up the operation corresponding to a type in the table, and returns the it or #f

;; this adds the collection of operations(aka package) specific to the rectangular representation of complex numbers to the complex number system
;; since we're defining everything internally, the names of the functions won't be added to the global scope, so we don't have to worry about not using the same function names
;; rectangular
(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z)
          (real-part z)))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  ;;   method     type
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; polar
(define (install-polar-package)
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang x y)
    (cons x y))
  (define (make-from-real-img r a)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; we've created the packages for the different representations, but we need a way to acces a more generic version, that will create a complex number regardless of the representation we choose
(define (apply-generic op . args)
  (let ((type-args (map type-tag args))) ; gets all types
    (let ((proc (get op type-tags)))     ; gets the operations corresponding to the types
      (if proc                           ; if it's found
        (apply proc (map contents args)) ; apply it to the arguments
        (error "No method for these types: APPLY-GENERIC"
               (list op type-tags))))))

;; apply-generic acts as a kind of search-and-apply; now, we define our generic operations in terms of it
(define (real-part z) (apply-generic 'real-part z)) ; searches for the operation 'real-part in the type of z, and calls it
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-magl-ang 'polar) r a))

