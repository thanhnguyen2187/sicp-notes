; By comparing the stack operations used by compiled code to the stack
; operations used by the evaluator for the same computation, we can determine
; the extent to which the compiler optimizes use of the stack, both in speed
; (reducing the total number of stack operations) and in space (reducing the
; maximum stack depth). Comparing this optimized stack use to the performance of
; a special-purpose machine for the same computation gives some indication of
; the quality of the computer.
;
; a. Exercise 5.27 asked you to determine, as a function of *n*, the number of
; pushes and the maximum stack depth needed by the evaluator to compute *n!*
; using the recursive factorial procedure given above. Exercise 5.14 asked you
; to do the same measurements for the special-purpose factorial machine in
; Figure 5.11. Now perform the same analysis using the compiled *factorial
; procedure.
;
; Take the ratio of the number pushes in the compiled version to the number of
; pushes in the interpreted version, and do the same for the maximum stack
; depth. Since the number of operations and the stack depth used to compute *n!*
; are linear in *n*, these ratios should approach constants as *n* becomes
; large. What are these constants? Similarly, find the ratios of the stack usage
; in the special-purpose machine to the usage interpreted version.
;
; Compare the ratios for special-purpose versus interpreted code to the ratios
; for compiled versus interpreted code. You should find that the special-purpose
; machine does much better than the compiled code, since the hand-tailored
; controller should be much better than what is produced by our rudimentary
; general-purpose compiler.
;
; b. Can you suggest improvements to the compiler that would help it generate
; code that would come closer in performance to the hand-tailored version?
;
; ---
;
; For total pushes:
;
; | n | interpreted  | compiled     | special-purpose |
; | - | ------------ | ------------ | --------------- |
; | 1 |           16 |            7 |               0 |
; | 2 |           48 |           13 |               2 |
; | 3 |           80 |           19 |               4 |
; | 4 |          112 |           25 |               6 |
; | 5 |          144 |           31 |               8 |
; | 6 |          176 |           37 |              10 |
; | 7 |          208 |           43 |              12 |
; | 8 |          240 |           49 |              14 |
;
; interpreted-total-pushes = (+ -16 (* 32 n))
; compiled-total-pushes = 1 + (* 6 n)
; special-purpose-pushes = (* 2 (- n 1))
; ratio-compiled-interpreted = (/ 6 32) = 0.1875
; ratio-special-purpose-interpreted = (/ 2 32) = 0.0625
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
;
; interpreted-maximum-depth = (+ 3 (* 5 n))
; compiled-maximum-depth = (+ -1 (* 3 n))
; special-purpose-maximum-depth = (* 2 (- n 1))
; ratio-compiled-interpreted = (/ 3 5) = 0.6
; ratio-special-purpose-compiled = (/ 2 5) = 0.4
; 
; b. Maybe one such optimization that was applied is in exercise 5.38, where we
; *open-code*, or inline primitives can work in this case.

(load "ch5-compiler")
(load "load-eceval-compiler")
(load "format-compiled-code")

(define fact-code
  '(define (factorial n)
     (if (= n 1)
       1
       (* (factorial (- n 1)) n))))

(compile-and-go fact-code)

(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)
(factorial 6)
(factorial 7)
(factorial 8)

(format-compiled-code (compile fact-code 'val 'next))

;; compiled code
((env)
 (val)
(
  (assign val (op make-compiled-procedure) (label entry70) (reg env))
  (goto (label after-lambda69))
entry70
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch85))
compiled-branch84
  (assign continue (label after-call83))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch85
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call83
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch72))
true-branch73
  (assign val (const 1))
  (goto (reg continue))
false-branch72
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch76))
compiled-branch75
  (assign continue (label after-call74))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch76
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call74
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch79))
compiled-branch78
  (assign continue (label after-call77))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch79
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call77
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch82))
compiled-branch81
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch82
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call80
after-if71
after-lambda69
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
))

;; special-purpose code
(controller
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
 fact-done)
