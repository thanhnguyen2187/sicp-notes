; Measure the number of pushes and the maximum stack depth required to compute
; `n!` for various small values of `n` using the factorial machine shown in
; Figure 5.11. From your data determine formulas in terms of `n` for the total
; number of pushing operations and the maximum stack depth used in computing
; `n!` for any `n > 1`. Note that each of these is a linear function of `n` and
; thus is determined by two constants.
;
; In order to get the statistics printed, you will have to augment the factorial
; machine with instructions to initialize the stack and print the statistics.
; You may want to also modify the machine so that it repeatedly reads a value
; for `n`, computes the factorial, and prints the result (as we did for the
; `GCD` machine in Figure 5.4), so that you will not have to repeatedly invoke
; `get-register-contents`, `set-register-contents!`, and `start`.

(load "ch5-regsim")

;; testing

;;FIGURE 5.11
(define fact-controller-text
  '(controller
      ; (assign n (op read))
      (assign continue (label fact-done))     ; set up final return address
    fact-loop
      (test (op =) (reg n) (const 1))
      (branch (label base-case))
      ;; Set up for the recursive call by saving n and continue.
      ;; Set up continue so that the computation will continue
      ;; at after-fact when the subroutine returns.
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (goto (label fact-loop))
    after-fact
      (restore n)
      (restore continue)
      (assign val (op *) (reg n) (reg val))   ; val now contains n(n-1)!
      (goto (reg continue))                   ; return to caller
    base-case
      (assign val (const 1))                  ; base case: 1!=1
      (goto (reg continue))                   ; return to caller
    fact-done))

(define fact-ops
  (list
    (list '= =)
    (list '- -)
    (list '* *)
    (list 'read read)))

(define fact-registers
  '(n continue val))

(map (lambda (n)
       (define fact-machine
         (make-machine
           fact-registers
           fact-ops
           fact-controller-text))
       (set-register-contents! fact-machine 'n n)
       (start fact-machine)
       ((fact-machine 'stack) 'print-statistics)
       (display " n = ") (display n))
     (list 1 2 3 4 5 6 7 8 9))

; | n | total-pushes | maximum-depth |
; | 1 | 0            | 0             |
; | 2 | 2            | 2             |
; | 3 | 4            | 4             |
; | 4 | 6            | 6             |
; | 5 | 8            | 8             |
; | 6 | 10           | 10            |
;
; The forumlae is: f(n) = (n - 1) * 2

