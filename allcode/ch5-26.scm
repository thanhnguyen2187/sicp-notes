; Use the monitored stack to explore the tail-recursive property of the
; evaluator (Section 5.4.2). Start the evaluator and define the iterative
; `factorial` procedure from Section 1.2.1:
;
; ...
;
; Run the procedure with some small values of *n*. Record the maximum stack
; depth and the number of pushes to compute *n!* for each of these values.
;
; a. You will find that the maximum depth required to evaluate *n!* is
; independent of *n*. What is that depth?
; b. Determine from your data a formula in terms of *n* for the total number of
; push operations used in evaluating *n!* for any *n* >= 1. Note that the number
; of operations used is a linear function of *n* and is thus determined by two
; constants.
;
; ---
;
; a. The maximal depth is `10`
; b.
;
; | n | total-pushes | maximum-depth |
; | 0 |           29 |             8 |
; | 1 |           64 |            10 |
; | 2 |           99 |            10 |
; | 3 |          134 |            10 |
; | 4 |          169 |            10 |
; | 5 |          204 |            10 |
; | 6 |          239 |            10 |
; | 7 |         5040 |            10 |
;
; total-pushes = (+ 29 (* 45 n))

(load "load-eceval.scm")

(define the-global-environment (setup-environment))

(start eceval)

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product) (+ counter 1))))
  (iter 1 1))

(factorial 0)
