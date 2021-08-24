; Exercise 2.70
;
; The following eight-symbol alphabet with associated relative frequencies was
; designed to efficiently encode the lyrics of 1950s rock songs. (Note that the
; "symbols" of an "alphabet" need not to be individual letters.)
;
; Use `generate-huffman-tree` (Exercise 2.69) to generate a corresponding
; Huffman tree, and use `encode` (Exercise 2.68) to encode the following
; message:
;
; Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na
; Wah yip yip yip yip yip yip yip yip yip Sha boom
; 
; How many bits are required for the encoding? What is the smallest number of
; bits that would be needed to encode this song if we use a fixed-length code
; for the eight-symbol alphabet?
; 
; ---
;
; 84 bits are required for this encoding. Under a fixed-length code, 36 * 3 =
; 108 bits are needed.

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
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (within? symbol symbol-list)
  (if (null? symbol-list)
      false
      (or (eq? symbol (car symbol-list))
          (within? symbol (cdr symbol-list)))))

(define (encode-symbol symbol tree)
  ; Return the list of bits that encodes a given symbol according to a given
  ; tree. It signals an error if the symbol is not in the tree at all.
  ; (display symbol) (newline)
  ; (display tree) (newline)
  ; (newline)
  (if (or (and (leaf? tree)
               (not (eq? (symbol-leaf tree)
                         symbol)))
          (not (within? symbol
                        (symbols tree))))
      (error "bad symbol ENCODE-SYMBOL" symbol)
      (let ((left-leaf  (left-branch  tree))
            (right-leaf (right-branch tree)))
        (cond ((and (leaf? left-leaf)
                    (eq? symbol
                         (symbol-leaf left-leaf)))
               (list 0))
              ((within? symbol
                        (symbols left-leaf))
               (cons 0 (encode-symbol symbol left-leaf)))
              ((and (leaf? right-leaf)
                    (eq? symbol
                         (symbol-leaf right-leaf)))
               (list 1))
              ((within? symbol
                        (symbols right-leaf))
               (cons 1 (encode-symbol symbol right-leaf)))
              (else (error "bad tree structure ENCODE-SYMBOL" tree)))))
  )

(define (successive-merge leaves)
  ; Successively merge the smallest-weight elements of the set until there is
  ; only one element left.
  (define (recurse leaves)
    (let ((leaf1   (car leaves))
          (remains (cdr leaves)))
      (if (null? remains)
          leaf1
          (let ((leaf2 (cadr leaves)))
            (successive-merge (adjoin-set (make-code-tree leaf1 leaf2)
                                          (cdr remains))))
          )))
  (recurse leaves))

(define (generate-huffman-tree pairs)
  ; Takes as its argument a list of symbol-frequency pairs, and generates a
  ; Huffman encoding tree according to the Huffman algorithm.
  (successive-merge (make-leaf-set pairs)))

; (define L '((A    2)
;             (BOOM 1)
;             (GET  2)
;             (JOB  2)
;             (SHA  3)
;             (NA   16)
;             (WAH  1)
;             (YIP  9)))
; (define L '((A 1)
;             (B 2)
;             (C 4)
;             (D 8)
;             (E 16)))
(define L '((A 1)
            (B 2)
            (C 4)
            (D 8)
            (E 16)
            (F 32)
            (G 64)
            (H 128)
            (I 256)
            (J 512)))
(display (make-leaf-set L)) (newline)
(define tree (generate-huffman-tree L))
; (define message '(
;                   GET A   JOB
;                   SHA NA  NA  NA  NA  NA  NA  NA  NA
;                   GET A   JOB
;                   SHA NA  NA  NA  NA  NA  NA  NA  NA
;                   WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
;                   SHA BOOM
;                   ))
(display tree) (newline)

; (define encoded-message (encode message tree))
; (length encoded-message)
; (map (lambda (symbol)
;        ; (display symbol) (newline)
;        (encode-symbol symbol tree)
;        )
;      message)
; (display message)
