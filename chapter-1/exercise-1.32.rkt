; Exercise 1.32
;
; a. Show that `sum` and `product` are both special cases of a still more
; general notion called `accumulate` that combines a collection of terms, using
; some general accumulation function:
; 
; ...
;
; `accumulate` takes as agurments the same term and range specifications as
; `sum` and `product`, together with a `combiner` procedure (of two arguments)
; that specifies how the current term is to be combined with the accumulation of
; the preceding terms and a `null-value` that specifies what base value to use
; when the terms run out. Write `accumulate` and show how `sum` and `product`
; can both be defined as simple calls to `accumulate`.
;
; b. If your `accumulate` procedure generates a recursive process, write one
; that generates an iterative process. If it genrates an iterative process,
; write one that generate a recursive process.

#lang sicp

; (define (accumulate combiner
;                     null-value
;                     term
;                     a
;                     next
;                     b)
;   (define (iter a result)
;     (if [> a b]
;         result
;         (iter (next a)
;               (combiner (term a)
;                         result))))
;   (iter a null-value))

(define (accumulate combiner
                    null-value
                    term
                    a
                    next
                    b)
  (if [> a b]
    null-value
    (combiner (term a)
              (accumulate combiner
                          null-value
                          term
                          (next a)
                          next
                          b))))

(define (sum-normal a b)
  (accumulate +
              0
              identity
              a
              inc
              b))

(define (factorial n)
  (accumulate *
              1
              identity
              1
              inc
              n))

(sum-normal 1 10)
(factorial 10)
