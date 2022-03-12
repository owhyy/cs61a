#lang racket
;; 2.55
(car ''abracadabra)
;; this is saying "create a pair, with the words ' and 'abracadabra
;; so (car (cons ' 'abracadabra)) will return the quote