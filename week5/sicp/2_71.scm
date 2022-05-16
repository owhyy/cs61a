#lang racket
(require rackunit)

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      null
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    null
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)
                             (cadr pair))
                  (make-leaf-set (cdr pairs))))))
;; ---

;; 2.68
(define (encode-symbol symbol tree)
  (cond ((leaf? tree)
         (if (eq? symbol (symbol-leaf tree))
           '()
           (error "symbol not in tree: encode-symbol")))
        ((memq symbol (symbols (left-branch tree))) (cons '0 (encode-symbol symbol (left-branch tree))))
        ((memq symbol (symbols (right-branch tree))) (cons '1 (encode-symbol symbol (right-branch tree))))))

(define (encode message tree)
  (if (null? message)
    null
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))
;; ---

;; 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond ((empty? (cdr leaf-set)) (car leaf-set))
        (else (successive-merge
                (adjoin-set (make-code-tree
                              (car leaf-set)
                              (cadr leaf-set))
                            (cddr leaf-set))))))
;; ---

;; 2.71
;; suppose we have an alphabet of 5 symbols, where each symbol is a letter, and each letter has a frequency of 1, 2, 4, ... 2^n-1
(define alphabet-5s (list '(A 1) '(B 2) '(C 4) '(D 8) '(E 16)))
(define tree-5s (generate-huffman-tree alphabet-5s))
(define alphabet-5s-message '(A B C D E B C D E C D E C D E D E D E D E D E E E E E E E E E))
(encode alphabet-5s-message tree-5s)
;; huffman encoding, this takes 56 bits
;; fixed-number size encoding, this would take 31*5 = 155 bits
;; how it works is this: as levels go down(as weight decreases), number of bits/symbol raises.
;; on the last level (letters A and B, or the 2 symbols with the least weight), the bits/symbol is n-1 (maximum, in this case - 5-1=4)
;; on the first level (letter E), the bits/symbol is 1 (smallest)
;; for everything in between, the bits/symbol decreases, as the level goes up (the bigger the weight, the smaller the bits/symbol),
;; so for C you have 3, for D you have 2 and so on

;; in order to calculate the number of bits for huffman encoding, you'd have to do (bits/symbol * symbol) - 1
;; (-1 since we're starting from 1, not 2)
;; so, for the above message, you'd have to do ((1 * 4) + (2 * 4) + (4 * 3) + (8 * 2) + (16 * 1)) - 1 = 55
;; and for n = 10, you'd have (1 * 9) + (2 * 9) + ... + (512 * 1) - 1, or 2034 bits, which would normally take 511 * 10 = 5110 bits

;; tests
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define sample-result '(A D A B B C A))

(define tests
  (test-suite
    "test for the huffman encoding algorithm"
    (check-equal? (encode (decode sample-message sample-tree) sample-tree) sample-message "testing that encoding and then deconding produces the same message")
    (check-equal? (generate-huffman-tree (list '(A 4) '(B 2) '(C 1) '(D 1))) sample-tree "testing that generating a tree from the book values generates sample-tree")
    )) ;; keep in mind order does not matter for elements with same weight

(require rackunit/text-ui)
;; runs the test

(run-tests tests)
;; ---
