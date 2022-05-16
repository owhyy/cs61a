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

;; 2.70
(define alphabet (list '(A 2) '(GET 2) '(SHA 3) '(WAH 1) '(BOOM 1) '(JOB 2) '(NA 16) '(YIP 9)))
(define rock-tree (generate-huffman-tree alphabet))
(define rock-message '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
(encode rock-message rock-tree)
;; encoding using huffman takes 84 bits
;; there are 8 (2^3) possible symbols (GET, BOOM, A are all considered one symbol each),
;; so we can fit all of the symbols on 3 bits each.
;; there are 36 total symbols
;; encoding using a fixed-length code would take 36 * 3 bits/symbol = 108
;; ---

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
