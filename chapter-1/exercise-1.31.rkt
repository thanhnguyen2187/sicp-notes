; Exercise 1.31
;
; a. The `sum` procedure is only the simplest of a vast number of similar
; abstractions that can be captured as higher-order procedures.
;
; Write an analogous procedure called `product` that returns
; - the product
; - of the values
; - of a function at points
; - over a given range.
;
; Show how to define `factorial` in terms of `product`. Also use the `product`
; to compute approximations to /pi/ using the formula
;
; pi/4 = 2/3 * 4/3 * 4/5 * 6/5 * 6/7 * 8/7 ...
;
; b. If your product procedure generates a recursive process, write one that
; generates an iterative process. If it generate an iterative process, write one
; that generates a recursive process.

#lang sicp

; (define (product term start next end)
;   (define (iter start result)
;     (if (> start end)
;         result
;         (iter (next start)
;               (* (term start)
;                  result))))
;   (iter start 1))

(define (product term start next end)
  (if (> start end)
    1
    (* (term start)
       (product term
                (next start)
                next
                end))))

(define (factorial n)
  (product identity
           1
           inc
           n))

(define (approximate-pi n)
  (define (term x)
    (if (even? x)
        (/ (+ x 2)
           (+ x 1))
        (/ (+ x 1)
           (+ x 2))))
  (product term
           1.0
           inc
           n))

(factorial 3)
(factorial 4)
(factorial 5)

(* 4 (approximate-pi 100000))
