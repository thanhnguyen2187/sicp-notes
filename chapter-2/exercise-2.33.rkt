; Exercise 2.33
;
; Fill the missing expressions to complete the following definitions of some
; basic list-manipulation operations as accumulations:
;
; ...

#lang sicp

(define ? 0)
(define (accumulate operator
                    initial
                    sequence)
  (if (null? sequence)
      initial
      (operator (car sequence)
                (accumulate operator
                            initial
                            (cdr sequence)))))

(define (map p
             sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              sequence))
(define (append seq1
                seq2)
  (accumulate cons
              seq1
              seq2))
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0
              sequence))

(map inc (list 1 2 3 4))
(append (list 1 2 3)
        (list 4 5 6))
(length (list 1 2 3 4 5))
