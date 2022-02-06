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

;; modified: added X1 and X2 as joker 1 and 2 correspondingly
(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se 'X1 'X2 (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

;; modified: changed size of deck from 52 to 54
(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
          (se (first in) (shuffle (se (bf in) out) (- size 1)))
          (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
        deck
        (move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 54) )

;; simple unit testing
(define (check-expect val result)
  (if (equal? val result)
      '(Test passed)
      (se '(Test failed: given)
          (if (boolean? val)
              (if (false? val) 'false 'true)
              (word val))
          '(expected)
          (if (boolean? result)
              (if (false? result) 'false 'true)
              (word result)))))

;; 1
;; modified: added calculation rule for joker
(define (best-total hand)
  (define (loop hand sum-so-far)
    (cond ((empty? hand) sum-so-far)
          ((member? (bl (first hand)) '(X x))
           (let ((new-sum (- 21 (loop (bf hand) sum-so-far))))
             (if (> new-sum 11) (+ 11 (- 21 new-sum)) (+ new-sum (- 21 new-sum)))))
          ((member? (bl (first hand)) '(A a))
           (let ((new-sum (+ 11 (loop (bf hand) sum-so-far))))
             (if (> new-sum 21) (- new-sum 10) new-sum)))
          ((member? (bl (first hand)) '(K Q J k q j))
           (loop (bf hand) (+ sum-so-far 10)))
          (else (loop (bf hand) (+ sum-so-far (butlast (first hand)))))))
  (loop hand 0))

(check-expect (best-total '(ad 8s)) 19)
(check-expect (best-total '(x1)) 11)
(check-expect (best-total '(x1 ad)) 21)
(check-expect (best-total '(ad x1 x2)) 22) ;bust
(check-expect (best-total '(x1 x2)) 21)
(check-expect (best-total '(x1 2s)) 13)
(check-expect (best-total '(ad 9h x1)) 21)
(check-expect (best-total '(ad 9h x1 x2)) 22) ;bust
(check-expect (best-total '(ad 8s 5h)) 14)
(check-expect (best-total '(ad as 9h)) 21)
(check-expect (best-total '(ad as ah)) 13)
(check-expect (best-total '(ad as kh)) 22) ;bust

;; 2
(define (stop-at-17 hand dealer-card)
  (< (best-total hand) 17))

(check-expect (stop-at-17 '(ad 8s) 'as) #f) ;19 -> stop taking
(check-expect (stop-at-17 '(ad 8s 5h) 'as) #t) ;14 -> can take
(check-expect (stop-at-17 '(ad 6s) 'as) #f) ;17 -> stop taking

;; 3
(define (play-n strategy n)
  (define (play win lose i)
    (if (> i n) (- win lose)
        (cond ((= (twenty-one strategy) 1) (play (+ win 1) lose (+ i 1)))
              ((= (twenty-one strategy) -1) (play win (+ lose 1) (+ i 1)))
              (else (play win lose (+ i 1))))))
  (play 0 0 0))

;; there is no way to unit test this, as the hand is randomly generated
;; trust me, it works :)

;; 4
;; modified: joker is now a face card too
(define (dealer-sensitive hand dealer-card)
  (cond ((and (member? (bl dealer-card) '(X A K Q J x a k q j 10 7 8 9))
              (< (best-total hand) 17)) #t)
        ((and (member? (bl dealer-card) '(2 3 4 5 7))
              (< (best-total hand) 12)) #t)
        (else #f)))

(check-expect (dealer-sensitive '(ad 8s) 'as) #f) ;>17 and dealer has big card -> stop taking
(check-expect (dealer-sensitive '(ad 8s) '2s) #f) ;>17 and dealer has small -> stop taking
(check-expect (dealer-sensitive '(ad 4s) 'as) #t) ;>12 <17 and dealer has big card -> continue taking
(check-expect (dealer-sensitive '(ad 4s) '2s) #f) ;>12 <17 and dealer has small card -> stop taking
(check-expect (dealer-sensitive '(6s) '2s) #t) ;<12 and dealer has big card -> continue taking
(check-expect (dealer-sensitive '(6s) '2s) #t) ;<12 and dealer has small -> continue taking

;; 5
(define (stop-at n)
  (lambda (hand dealer-card) (< (best-total hand) n)))

(check-expect ((stop-at 17) '(ad 8s) 'as) #f) ;19 -> stop taking
(check-expect ((stop-at 17) '(ad 8s 5h) 'as) #t) ;14 -> can take
(check-expect ((stop-at 17) '(ad 6s) 'as) #f) ;17 -> stop taking

;; 6
(define (valentine hand dealer-card)
  (define (contains-hearts? current-hand)
    (cond ((empty? current-hand) #f)
          ((member? (last (first current-hand)) '(h H)) #t)
          (else (contains-hearts? (bf current-hand)))))
  (if (contains-hearts? hand)
      ((stop-at 19) hand dealer-card)
      ((stop-at 17) hand dealer-card)))

(check-expect (valentine '(ad 7s) 'as) #f) ;>17 and no hearts -> stop taking
(check-expect (valentine '(ad 7h) 'as) #t) ;>17 and one heart -> continue taking
(check-expect (valentine '(ad 9s) 'as) #f) ;>19 and no heart -> stop taking
(check-expect (valentine '(ad 9h) 'as) #f) ;>19 and one heart -> stop taking
(check-expect (valentine '(5d 3d) 'as) #t) ;<17 and no heart -> continue taking
(check-expect (valentine '(5d 3h) 'as) #t) ;<17 and one heart -> continue taking
(check-expect (valentine '(10d 3h) 'as) #t) ;<17 and one heart -> continue taking

;; 7
(define (suit-strategy suit strat-alt strat)
  (lambda (hand dealer-card)
    (define (contains-suit? current-hand)
      (cond ((empty? current-hand) #f)
            ((equal? (last (first current-hand)) suit) #t)
            (else (contains-suit? (bf current-hand)))))
    (if (contains-suit? hand)
        (strat hand dealer-card)
        (strat-alt hand dealer-card))))

(check-expect ((suit-strategy 'h (stop-at 17) (stop-at 19)) '(ad 7s) 'as) #f)
(check-expect ((suit-strategy 'h (stop-at 17) (stop-at 19)) '(ad 7h) 'as) #t)
(check-expect ((suit-strategy 'h (stop-at 17) (stop-at 19)) '(ad 9s) 'as) #f)
(check-expect ((suit-strategy 'h (stop-at 17) (stop-at 19)) '(ad 9h) 'as) #f)
(check-expect ((suit-strategy 'h (stop-at 17) (stop-at 19)) '(5d 3d) 'as) #t)
(check-expect ((suit-strategy 'h (stop-at 17) (stop-at 19)) '(5d 3h) 'as) #t)
(check-expect ((suit-strategy 'h (stop-at 17) (stop-at 19)) '(10d 3h) 'as) #t)

;; 8
(define (true? pred) (not (false? pred)))
(define (majority s1 s2 s3)
  (lambda (hand dealer-card)
    (let ((r1 (s1 hand dealer-card))
          (r2 (s2 hand dealer-card))
          (r3 (s3 hand dealer-card)))
      (if (or (and (true? r1) (true? r2))
              (and (true? r1) (true? r3))
              (and (true? r2) (true? r3)))
          #t
          #f))))

(check-expect ((majority stop-at-17 dealer-sensitive valentine) '(ad 7s) 'as) #f) ; 18 -> stop-at-17: #f, dealer-sensitive: #f, valentine: #f -> #f
(check-expect ((majority stop-at-17 dealer-sensitive valentine) '(3d 7s) 'as) #t) ; 10 -> stop-at-17: #t, dealer-sensitive: #t, valentine: #t -> #t
(check-expect ((majority stop-at-17 dealer-sensitive valentine) '(10d 7h) 'as) #f) ; 17 -> stop-at-17: #f, dealer-sensitive: #f, valentine: #t -> #f

;; 9
(define (reckless s)
  (lambda (hand dealer-card) ((lambda () (s hand dealer-card)))))

(check-expect ((reckless stop-at-17) '(ad 7s) 'as) #f) ; 17
(check-expect ((reckless valentine) '(ad 2s ah) 'as) #t)