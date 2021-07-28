; Exercise 1.39
;
; A continued fraction representation of the tangent function was published in
; 1770 by the German mathematician J. H. Lamber:
;
; tan(x) = x/(1 - x^2/(3 - x^2/(5 -(x^2))))
;
; where /x/ is in radians.
;
; Define a procedure `(tan-cf x k)` that computes an approximation to the
; tangent function based on Lambert's formula. `k` specifies the number of terms
; to compute, as in Exercise 1.37.

#lang sicp

(define (cont-frac n
                   d
                   k)
  (define (recurse index)
    (if (= index k)
        (/ (n index)
           (d index))
        (/ (n index)
           (+ (d index)
              (recurse (+ index 1))))))
  (define (iterate index result)
    ; (display result)
    ; (newline)
    (if (= index 0)
        result
        (iterate (- index 1)
                 (/ (n index)
                    (+ result
                       (d index))))))
  ; (iterate k 0)
  (recurse 1)
  )

(define (tan-cf x k)
  (define (n index)
    (if (= index 1)
        x
        (- (* x x))))
  (define (d index)
    (- (* 2 index)
       1))
  (cont-frac n
             d
             k))

(tan-cf 3.0 100)
(tan 3)

