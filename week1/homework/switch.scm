#lang racket
(require berkeley)

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
