; Exercise 3.39
;
; Which of the five possibilities in the parallel execution shown above remain
; if we instead serialize execution as follows:
;
; ...
;
; ---
;
; The possibilities are: 101, 121, and 11, and 100.
;
; 1. set! x
; 2. (* x x)
; 3. set! x (+ x 1)

#lang sicp

(define x 10)
(define s (make-serializer))
(parallel-execute
  (lambda () (set! x ((s (lambda () (* x x))))))
  (s (lambda () (set! x (+ x 1)))))
