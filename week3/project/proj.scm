#lang racket
(require berkeley)

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
          ((< (best-total dealer-hand-so-far) 17)
           (play-dealer customer-hand
                        (se dealer-hand-so-far (first rest-of-deck))
                        (bf rest-of-deck)))
          ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
          ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
          (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
          ((strategy customer-hand-so-far dealer-up-card)
           (play-customer (se customer-hand-so-far (first rest-of-deck))
                          dealer-up-card
                          (bf rest-of-deck)))
          (else
           (play-dealer customer-hand-so-far
                        (se dealer-up-card (first rest-of-deck))
                        (bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
                   (first (bf (bf deck)))
                   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
          (se (first in) (shuffle (se (bf in) out) (- size 1)))
          (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
        deck
        (move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

;; simple unit testing function
(define (check-expect fn ans)
  (= fn ans))


(define (best-total hand)
  (define (loop hand sum-so-far)
    (cond ((empty? hand) sum-so-far)
          ((member? (bl (first hand)) '(A a))
           (let ((new-sum (+ 11 (loop (bf hand) sum-so-far))))
             (if (> new-sum 21) (- new-sum 10) new-sum)))
          ((member? (bl (first hand)) '(K Q J k q j))
           (loop (bf hand) (+ sum-so-far 10)))
          (else (loop (bf hand) (+ sum-so-far (butlast (first hand)))))))
  (loop hand 0))

(best-total '(as 7s))

(define (stop-at-17 hand dealer-card)
    (if (< (best-total hand) 17) #t #f))

(twenty-one stop-at-17)

(define (play-n strategy n)
  (define (play win lose i)
    (if (> i n) (- win lose)
        (cond ((= (twenty-one strategy) 1) (play (+ win 1) lose (+ i 1)))
              ((= (twenty-one strategy) -1) (play win (+ lose 1) (+ i 1)))
              (else (play win lose (+ i 1))))))
  (play 0 0 0))

(play-n stop-at-17 10)

;(define (play-n strategy n)
;  (define (play won lost i)
;    (if (= i n) (- won lost)
;        (if (strategy make-deck)
;            (play (+ won 1) lost (+ i 1))
;            (play won (+ lost 1) (+ i 1)))))
;  (play 0 0 0))