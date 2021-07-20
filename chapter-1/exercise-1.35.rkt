; Exercise 1.35
;
; Show that the golden ratio /phi/ is a fixed point of the transformation x |->
; 1+ 1/x, and use this fact to compute /phi/ by means of the `fixed-point`
; procedure.

#lang sicp

(define tolerance 0.00001)

(define (fixed-point function
                     first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (function guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; (fixed-point cos 1.0)
(fixed-point (lambda (x)
               (+ 1
                  (/ 1 x)))
             1.0)
