; Carry out an analysis like the one in Exercise 5.45 to determine the
; effectiveness of compiling the tree-recursive Fibonacci procedure
;
; ...
;
; compared to the effectiveness of using the special-purpose Fibonacci machine
; of Figure 5.12. (For measurement of the interpreted performance, see Exercise
; 5.29.) For Fibonacci, the time resource used is not linear in *n*; hence the
; ratios of stack operations will not approach a limiting value that is
; independent of *n*.
;
; ---
;
; For total pushes:
;
; | n | interpreted  | compiled     | special-purpose |
; | - | ------------ | ------------ | --------------- |
; | 1 |           16 |            7 |               0 |
; | 2 |           72 |           17 |               4 |
; | 3 |          128 |           27 |               8 |
; | 4 |          240 |           47 |              16 |
; | 5 |          408 |           77 |              28 |
; | 6 |          688 |          127 |              48 |
; | 7 |         1136 |          207 |              80 |
; | 8 |         1864 |          337 |             132 |
;
; For maximum depth:
;
; | n | interpreted   | compiled      | special-purpose |
; | - | ------------- | ------------- | --------------- |
; | 1 |             8 |             3 |               0 |
; | 2 |            13 |             5 |               2 |
; | 3 |            18 |             8 |               4 |
; | 4 |            23 |            11 |               6 |
; | 5 |            28 |            14 |               8 |
; | 6 |            33 |            17 |              10 |
; | 7 |            38 |            20 |              12 |
; | 8 |            43 |            23 |              14 |

(load "ch5-compiler")
(load "load-eceval-compiler")
(load "format-compiled-code")

(define fib-code
  '(define (fib n)
     (if (< n 2)
       n
       (+ (fib (- n 1))
          (fib (- n 2))))))

(start-eceval)

(compile-and-go fib-code)

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)

(define fib-controller-text
  '(;; hard code n's value for faster testing
    ; (assign n (op read))
    ; n should be provided beforehand
    (assign continue (label fib-done))
    fib-loop
    (test (op <) (reg n) (const 2))
    (branch (label immediate-answer))
    ;; set up to compute Fib(n-1)
    (save continue)
    (assign continue (label afterfib-n-1))
    (save n)                           ; save old value of n
    (assign n (op -) (reg n) (const 1)); clobber n to n-1
    (goto (label fib-loop))            ; perform recursive call
    afterfib-n-1                       ; upon return, val contains Fib(n-1)
    (restore n)
    (restore continue)
    ;; set up to compute Fib(n-2)
    (assign n (op -) (reg n) (const 2))
    (save continue)
    (assign continue (label afterfib-n-2))
    (save val)                         ; save Fib(n-1)
    (goto (label fib-loop))
    afterfib-n-2                       ; upon return, val contains Fib(n-2)
    (assign n (reg val))               ; n now contains Fib(n-2)
    (restore val)                      ; val now contains Fib(n-1)
    (restore continue)
    (assign val                        ; Fib(n-1)+Fib(n-2)
            (op +) (reg val) (reg n)) 
    (goto (reg continue))              ; return to caller, answer is in val
    immediate-answer
    (assign val (reg n))               ; base case: Fib(n)=n
    (goto (reg continue))
    fib-done))

(define fib-ops
  (list
    (list '- -)
    (list '+ +)
    (list '< <)
    (list 'read read)))

(define fib-registers
  '(n continue val))

(map (lambda (n)
       (define fib-machine
         (make-machine
           fib-registers
           fib-ops
           fib-controller-text))
       (set-register-contents! fib-machine 'n n)
       (start fib-machine)
       ((fib-machine 'stack) 'print-statistics)
       (display " n = ") (display n))
     (list 1 2 3 4 5 6 7 8 9))

