; Exercise 2.45
;
; `right-split` and `up-split` can be expressed as instances of a general
; splitting operation. Define a procedure `split` with the property that
; evaluating 
;
; ...
;
; produces procedures `right-split` and `up-split` with the same behaviors as
; the ones already defined.

#lang sicp

(define (beside p1 p2)
  ; takes two painters and produces a new, compound painter
  ; that draws:
  ; - the first painter's image in the left half of the frame 
  ; - the second painter's image in the right half of the frame
  #t
  )
(define (below p1 p2)
  ; takes to painters and produce a compound painter that draws:
  ; - the first painter's image below the second painter's image
  #t
  )

(define right-split (split beside below))
(define up-split (split below beside))

(define (split a1 a2)
  ; takes two actions and return a split action
  ; that consequencely applies the two
  (define (iterate painter n)
    (if (= n 0)
        painter
        (iterate (a1 painter
                     (a2 painter painter))
                 (- n 1))))
  (lambda (painter n) (iterate painter n)))

; (define (inc-n n)
;   (define (iterate x n)
;     (if (= n 0)
;         x
;         (iterate (inc x) (- n 1))))
;   (lambda (x) (iterate x n)))
; 
; (define inc-3 (inc-n 3))
; (inc-3 3)
