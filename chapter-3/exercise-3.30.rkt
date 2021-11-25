; Exercise 3.30
;
; Figure 3.27 shows a *ripple-carry adder* formed by stringing together *n*
; full-adders. This is the simplest form of parallel adder for adding two
; *n*-bit binary numbers. The inputs /A_1/, /A_2/, /A_3/, ..., /A_n/ and /B_1/,
; /B_2/, /B_3/, ..., /B_n/ are the two binary numbers to be added (each /A_k/
; and /B_k/ is a 0 or a 1).
;
; The circuit generates /S_1/, /S_2/, /S_3/, ..., /S_n/, the *n* bits of the
; sum, and /C/, the carry from the addition.
;
; Write a procedure `ripple-carry-adder` that generates this circuit.
;
; The procedure should take as arguments three lists of *n* wires each -- the
; /A_k/, the /B_k/, and the /S_k/ -- and also another wire /C/.
;
; The major drawback of the ripple-carry adder is the need to wait for the carry
; signals to propagate. What is the delay needed to obtain the complete output
; from an *n*-bit ripple-carry.
;
; The circuit generates /S_1/, /S_2/, /S_3/, ..., /S_n/, the *n* bits of the
; sum, and /C/, the carry from the addition.
;
; Write a procedure `ripple-carry-adder` that generates this circuit. The
; procedure should take as agruments three lists of /n/ wires each -- the /A_k/,
; the /B_k/, and the /S_k/ -- and also another wire /C/.
;
; The major drawback of the ripple-carry adder is the need to wait for the carry
; signals to propagate. What is the delay needed to obtain the complete output
; from an /nm/-bit ripple-carry adder, expressed in terms of the delays for
; and-gates, or-gates, and inverters?
;
; ---
;
; - `half-adder`: 1 `or`, 2 `and`s, and 1 `inverter`
; - `full-adder`: 2 `half-adder`, 1 `or`
;   -> 3 `or`s, 4 `and`s, and 1 `inverter`
; - `ripple-carry`: n `full-adders`
;   -> 3*n `or`s, 4*n `and`s, and n `inverter`

#lang sicp

(define (full-adder a b c-in sum c-out)
  (let ((s  (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 count)
    'ok))

(define (ripple-carry-adder A B c-in c-out S)
  (if (null? A)
      S
      (let ((a (car A))
            (b (car B))
            (c-out (make-wire))
            (sum (make-wire)))
        (full-adder a b c-in sum c-out)
        (set! S (cons sum S))
        (ripple-carry-adder (cdr A)
                            (cdr B)
                            c-out
                            (make-wire)))))
