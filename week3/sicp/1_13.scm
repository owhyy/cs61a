#lang racket
(define phi (/ (+ 1 (sqrt 5)) 2))
(define other_phi (/ (- 1 (sqrt 5)) 2))

;; to prove: Fib(n) = phi^n/sqrt(5)
;; induction: if Fib(n) = (phi^n-other_phi^n)/sqrt(5) then the above is true

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (power n e)
  (if (= e 0) 1
      (* n (power n (- e 1)))))
  
;; produces #t if Fib(n) = (phi^n-other_phi^n)/sqrt(5)
(define (proof n)
  (= (fib n)
     (/ (- (power phi n) (power other_phi n)) (sqrt 5))))
