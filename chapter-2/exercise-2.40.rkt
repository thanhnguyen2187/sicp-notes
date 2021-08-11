; Exercise 2.40
;
; Define a procedure `unique-pairs` that, given an integer /n/, generates the
; sequence of pair /(i, j)/ with /1 <= i < j < i <= n.
;
; Use `unique-pairs` to simplify the definition of `prime-sum-pairs` given
; above.

#lang sicp

(define (enumerate-interval low
                            high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1)
                                    high))))

(define (accumulate operator
                    initial
                    sequence)
  (if (null? sequence)
      initial
      (operator (car sequence)
                (accumulate operator
                            initial
                            (cdr sequence)))))

(define (flatmap procedure
                 sequence)
  (accumulate append
              nil
              (map procedure
                   sequence)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list j i))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(unique-pairs 5)

