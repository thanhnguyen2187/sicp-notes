; Exercise 3.29
;
; Another way to construct an or-gate is as compound digital logic device, built
; from and-gates and inverters. Define a procedure `or-gate` that accomplishes
; this. What is the delay time of the or-gate in terms of `and-gate-delay` and
; `inverter-delay`?
;
; ---
;
; Delay time of the new or-gate is 1 and-gate-delay, and 3 inverter-delay

#lang sicp

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (inverter (and-gate (inverter a1 a1)
                      (inverter a2 a2))
            output))
