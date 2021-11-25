; Exercise 3.40
;
; Give all possiblities values of `x` that can result from executing:
;
; ...
;
; Which of these possibilites remain if we instead use serialized procedures:
;
; It is 10^5.

#lang sicp

(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

; Fetch `x` for `* x x`   (1.1)
; Calculate `* x x`       (1.2)
; Mutate `x` to `* x x`   (1.3)
;
; Fetch `x` for `* x x x` (2.1)
; Calculate `* x x x`     (2.2)
; Mutate `x` to `* x x x` (2.3)

; 1.1 1.2 1.3 2.1 2.2 2.3
; 1.1 1.2 2.1 2.2 2.3 1.3
; 2 1 3 4 5 6
; x^2
; x^3
; x^5
