; Exercise 4.32
;
; Give some examples that illustrate the difference between the streams of
; Chapter 3 and the "lazier" lazy list described in this section. How can you
; take advantage of this extra laziness?

#lang sicp

(define (cons x y)
  (x (delay y)))

(delay 1)
