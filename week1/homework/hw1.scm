#lang racket
(require berkeley)

;; squares
(define (square x) (* x x))

(define (squares sent)
  (if (empty? sent) empty
      (se (square (first sent))
          (squares (bf sent)))))

(squares (se 1 2 3 4))

;; switch
(define (switch-aux sent)
  (cond ((empty? sent) empty)
        ((member? (first sent) '(I me i Me ME))
         (se 'you (switch-aux (bf sent))))
        ((member? (first sent) '(You you YOU))
         (se 'me (switch-aux (bf sent))))
        (else (se (first sent) (switch-aux (bf sent))))))

(define (add-i-to-beginning sent)
  (if (equal? (first sent) 'me)
      (se 'I (bf sent))
      sent))

(define (switch sent)
  (add-i-to-beginning (switch-aux sent)))

(add-i-to-beginning (switch '(You told me that I should wake you up)))

;; ordered
(define (less-first-bf f b)
  (cond ((empty? b) #t)
        ((<= f (first b))
         (less-first-bf f (bf b)))
        (else #f)))

(define (ordered? sent)
  (if (empty? sent) #t
      (and (less-first-bf (first sent) (bf sent))
           (ordered? (bf sent)))))

(ordered? '(2 3 4 5))

;; ends-e
(define (ends-e sent)
  (cond ((empty? sent) empty)
        ((equal? (last (first sent)) 'e)
         (se (first sent) (ends-e (bf sent))))
        (else (ends-e (bf sent)))))

(ends-e '(please put the salami above the blue elephant))
