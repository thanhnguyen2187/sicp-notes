; Exercise 4.18
;
; Consider an alternative strategy for scanning out definitions that scanning
; out definitions that translates the example in the text to
;
; ...
;
; Here `a` and `b` are meant to represent new variable names, created by the
; interpreter, that do not appear in the user's program. Consider the `solve`
; procedure from Section 3.5.4:
;
; ...
;
; Will this procedure work if internal definitions are scanned out as shown in
; the exercise? What is they are scanned out as shown in the text? Explain
;
; ---
;
; It is not going to work, since `stream-map` wants its second argument to be
; a stream, which is definitely not what `'*undefined*` is.

#lang sicp

(define <e1> 0)
(define <e2> 0)
(define <e3> 0)

(define stream-map 0)
(define integral 0)
(define delay 0)

(lambda <vars>
  (define u <e1>)
  (define v <e2>)
  <e3>)

(lambda <vars>
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (let ((a <e1>)
          (b <e2>))
      (set! u a)
      (set! v b)
      <e3>)))

(define (solve f y0 dt)
  (define y  (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (solve-transformed f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b))))
