; Monitor the stack operations in the tree-recursive Fibonacci computation:
;
; ...
;
; a. Give a formula in terms of *n* for the maximum depth of the stack required
;    to compute Fib(*n*) for *n >= 2*.
;
;    Hint: In section 1.2.2 we argued that the space used by this process grows
;    linearly with *n*.
;
; b. Give a formula for the total number of pushes used to compute Fib(*n*) for
;    *n* >= 2. You should find that the number of pushes (which correlates well
;    with the time used) grows exponentially with *n*.
;
;    Hint: Let *S(n)* be the number of pushes used in computing Fib(*n*). You
;    should be able to argue that there is a formula that expresses *S(n)* in
;    terms of *S(n - 1)*, *S(n - 2)*, and some fixed "overhead" constant *k*
;    that is independent of *n*. Give the formula, and say what *k* is. Then
;    show that *S(n)* can be expressed as *a * Fib(n + 1) + b* and give the
;    values of *a* and *b*.
;
; ---
;
; |  n | fib | fib + 1 | total-pushes | maximum-depth |
; | -- | --- | ------- | ------------ | ------------- |
; |  1 |   1 |       1 |           16 |             8 |
; |  2 |   1 |       2 |           72 |            13 |
; |  3 |   2 |       3 |          128 |            18 |
; |  4 |   3 |       5 |          240 |            23 |
; |  5 |   5 |       8 |          408 |            28 |
; |  6 |   8 |      13 |          688 |            33 |
; |  7 |  13 |      21 |         1136 |            38 |
; |  8 |  21 |      34 |         1864 |            43 |
; |  9 |  34 |      55 |         3040 |            48 |
; | 10 |  55 |      89 |         4944 |            53 |
; | 11 |  89 |     144 |         8024 |            58 |
; | 12 | 144 |         |        13008 |            63 |
;
; a. (+ (* 5 n) 3)
; b. (+ (S (- n 1)) (S (- n 2)) 40)
;    (- (* 56 (fib (+ n 1))) 40)

(load "load-eceval.scm")

(define the-global-environment (setup-environment))

(start eceval)

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)
(fib 10)
(fib 11)
(fib 12)

