; Exercise 2.69
;
; The following procedure takes as its argument a list of symbol-frequency pairs
; (where no symbol appears in more than one pair) and generates a Huffman
; encoding tree according to the Huffman algorithm.
;
; ...
;
; `make-leaf-set` is the procedure given above that transform the list of pairs
; into an ordered set of leaves. `successive-merge` is the procedure you must
; write, using `make-code-tree` to successively merge the smallest-weight
; elements of the set until there is only one element left, which is the desired
; Huffman tree.
;
; (This procedure is slightly tricky, but not really complicated. If you find
; yourself designing a complex procedure, then you are almost certainly doing
; something wrong. You can take significant advantage of the fact that we are
; using an ordered set representation.)

#lang sicp

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr  x))
(define (weight-leaf x) (caddr x))

(define (left-branch  tree) (car  tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left)
           (weight right))))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch  branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits)
                        next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x)
            (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  ; Take a list of symbol-frequency pairs such as `((A 4) (B 2) (C 1) (D 1))`
  ; and constructs an initial ordered set of leaves, ready to be merged.
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car  pair)  ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? tree)
      '()
      (append (encode-symbol (car message)
                             tree)
              (encode (cdr message)
                      tree))))

(define (within element sequence)
  (define (iterate sequence)
    (cond ((null? sequence) false)
          ((eq? (car sequence) element) true)
          (else (iterate (cdr sequence)))))
  (iterate sequence))

(define (encode-symbol symbol tree)
  ; Return the list of bits that encodes a given symbol according to a given
  ; tree. It signals an error if the symbol is not in the tree at all.
  (if (leaf? tree)
      (if (eq? (symbol-leaf tree) symbol)
          (list)
          (error "bad symbol: ENCODE-SYMBOL" symbol))
      (let ((left-leaf (left-branch tree)))
        (if (eq? (symbol-leaf left-leaf)
                 symbol)
            (list 0)
            (append (list 1)
                    (encode-symbol symbol
                                   (right-branch tree)))))))

(define (successive-merge leaves)
  ; Successively merge the smallest-weight elements of the set until there is
  ; only one element left.
  (define (recurse leaves)
    (let ((leaf1   (car  leaves))
          (leaf2   (cadr leaves))
          (remains (cddr leaves)))
      (if (null? remains)
          (make-code-tree leaf1 leaf2)
          (successive-merge (adjoin-set (make-code-tree leaf1 leaf2)
                                        remains)))))
  (recurse leaves))

(define (generate-huffman-tree pairs)
  ; Takes as its argument a list of symbol-frequency pairs, and generates a
  ; Huffman encoding tree according to the Huffman algorithm.
  (successive-merge (make-leaf-set pairs)))

(define L '((B 2) (A 4) (C 1) (D 1)))
(make-leaf-set L)
(generate-huffman-tree L)
