; For comparison with Exercise 5.26, explore the behavior of the following
; procedure for computing factorials recursively:
;
; ...
;
; By running this procedure with the monitored stack, determine, as a function
; of *n*, the maximum depth of the stack and the total number of pushes used in
; evaluating *n!* for *n* >= 1. (Again, these functions will be linear.)
; Summarize your experiments by filling in the following table with the
; appropriate expressions in terms of *n*:
;
; |           | Maximum depth | Number of pushes |
; | --------- | ------------- | ---------------- |
; | Recursive |               |                  |
; | factorial |               |                  |
; | --------- | ------------- | ---------------- |
; | Iterative |               |                  |
; | factorial |               |                  |
;
; The maximum depth is a measure of the amount of space used by the evaluator in
; carrying out the computation, and the number of pushes correlates well with
; the time required.
;
; ---
;
; Statistics for recursive version:
;
; | n | total-pushes | maximum-depth |
; | - | ------------ | ------------- |
; | 1 |           16 |             8 |
; | 2 |           48 |            13 |
; | 3 |           80 |            18 |
; | 4 |          112 |            23 |
; | 5 |          144 |            28 |
; | 6 |          176 |            33 |
; | 7 |          208 |            38 |
; | 8 |          240 |            43 |
;
; total-pushes = (+ -16 (* 32 n))
; maximum-depth = (+ 3 (* 5 n))
;
; |           | Maximum depth | Number of pushes |
; | --------- | ------------- | ---------------- |
; | Recursive | (+ 3 (* 5 n)) | (+ -16 (* 32 n)) |
; | factorial |               |                  |
; | --------- | ------------- | ---------------- |
; | Iterative | 10            | (+ 29 (* 35 n))  |
; | factorial |               |                  |

(load "load-eceval.scm")

(define the-global-environment (setup-environment))

(start eceval)

(define (factorial n)
  (if (= n 1)
    1
    (* (factorial (- n 1)) n)))

(factorial 7)

(define (f n)
  (+ -16 (* 32 n)))
