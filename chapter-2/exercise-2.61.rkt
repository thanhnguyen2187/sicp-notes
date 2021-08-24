; Exercise 2.61
;
; Give an implementation of `adjoin-set` using the ordered set representation.
; By analogy with `element-of-set?` show how to take advantage of the ordering
; to produce a proceduure that requires on the average about half of as many
; steps as with the unordered representation.

#lang sicp

(define (element-of-set? x
                         set)
  (cond ((null? set) false)
        ((= (car set)) true)
        ((< (car set)) false)
        (else (element-of-set? x
                                  (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (if (or (null? set1)
          (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons (x1 (intersection-set (cdr set1)
                                              (cdr set2)))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((> x1 x2)
               (intersection-set set1 (cdr set2)))))))
