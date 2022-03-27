#lang racket
(require berkeley)

; The repl

(define (calc)
  (display "calc: ")
  (flush-output)
  (print (calc-eval (read)))
  (calc))

; Evaluating an expression

(define (calc-eval exp)
  (cond ((number? exp) exp)
        ((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
        ((word? exp) exp)
        (else (error "Calc: bad expression:" exp))))

; Apply a function to arguments
(define (calc-apply fn args)
  (cond ((eq? fn '+) (foldr + 0 args))
        ((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
                           ((= (length args) 1) (- (car args)))
                           (else (- (car args) (foldr + 0 (cdr args))))))
        ((eq? fn '*) (foldr * 1 args))
        ((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
                           ((= (length args) 1) (/ (car args)))
                           (else (/ (car args) (foldr * 1 (cdr args))))))
        ((eq? fn 'first) (first (car args)))
        ((eq? fn 'butfirst) (butfirst (car args)))
        ((eq? fn 'last) (last (car args)))
        ((eq? fn 'butlast) (butlast (car args)))
        ((eq? fn 'word) (if (empty? args)
                            ""
                            (word (first args) (calc-apply 'word (butfirst args)))))
        (else (error "Calc: bad operator:" fn))))
