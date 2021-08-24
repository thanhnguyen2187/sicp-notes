; Exercise ?.?
;
; A Huffman code can be represented as a binary tree whose leaves are the
; symbols that are encoded.
;
; At each non-leaf node of the tree there is a set containing all the symbols in
; the leaves that lie below the node. In addition, each symbol at a leaf is
; assigned a weight (which is its relative frequency), and each non-leaf node
; contains a weight that is the sum of all the weights of the leaves lying below
; it.
;
; The algorithm for generating a Huffman tree is very simple. The idea is to
; arrange the tree so that the symbols with the lowest frequency appear farthest
; away from the root.
;
; - Begin with the set of leaf nodes, containing symbols and their frequencies,
; as determined by the initial data from which the code is to be constructed.
; - Now find two leaves with the lowest and merge them to produce a node that
; has these two nodes as its left and right branches. The weight of the new node
; is the sum of the two weights.
; - Remove the two leaves from the original set and replace them by this new
; node.
; - Now continue this process.
;

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
