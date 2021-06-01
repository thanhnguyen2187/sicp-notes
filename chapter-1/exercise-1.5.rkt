#lang sicp

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

; (test 0 (p))
; (if (= 0 0) 0 (p))
; (0)

; If the interpreter uses applicative-order evaluation,
; it will run forever.
; If the interpreter uses normal-order evaluation,
; it will return 0.
