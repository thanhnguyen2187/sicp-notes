; Exercise 2.39
;
; Complete the following definitions of `reverse` (Exercise 2.18) in terms of
; `fold-right` and `fold-left` from Exercise 2.38.
;
; ...

#lang sicp

(define (fold-left operator
                   initial
                   sequence)
  (define (iterate result
                   rest)
    (if (null? rest)
        result
        (iterate (operator result (car rest))
                 (cdr rest))))
  (iterate initial
           sequence))

(define (fold-right operator
                    initial
                    sequence)
  (if (null? sequence)
      initial
      (operator (car sequence)
                (fold-right operator
                            initial
                            (cdr sequence)))))

(define (reverse sequence)
  (fold-right (lambda (x y)
                (append y
                        (list x)))
              nil
              sequence))

(define (reverse-v2 sequence)
  (fold-left (lambda (x y)
               (append (list y)
                       x))
              nil
              sequence))

(define L (list 1 2 3 4 5))
(reverse L)
(reverse-v2 L)
