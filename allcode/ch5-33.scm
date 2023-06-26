; Consider the following definition of a factorial procedure, which is slightly
; different from the one given above:
;
; ...
;
; Compile this procedure and compare the resulting code with that produced for
; `factorial`. Explain any differences you find. Does either program execute
; more efficiently than the other?
;
; ---
;
; The difference is between whether `env` and `argl` get saved and restored
; from the stack in `false-branch`. The original version saves and restores
; `argl`, while the other saves and restores `env`, which creates no difference
; at all.

(load "ch5-compiler.scm")

(compile
  '(define (factorial n)
     (if (= n 1)
       1
       (* (factorial (- n 1)) n)))
  'val
  'next)

((env)
 (val)
 (;; construct the procedure and skip over code for the procedure body
  (assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
entry2 ;; calls to factorial will enter here
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  ;; begin actual procedure body
  (save continue)
  (save env)
  ;; compute (= n 1)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch17))
compiled-branch16
  (assign continue (label after-call15))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch17
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call15 ; `val` now contains result of `(= n 1)`
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch4))
true-branch5 ; return 1
  (assign val (const 1))
  (goto (reg continue))
false-branch4
;; compute and return `(* (factorial (- n 1)) n)`
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue)
  (save proc) ; save `*` procedure
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (save argl) ; save partial argument list for `*`
  ;; compute `(factorial (- n 1))`, which is the other argument for `*`
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch8))
compiled-branch7
  (assign continue (label after-call6))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch8
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call6 ; `val` now contains result of `(- n 1)`
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch11))
compiled-branch10
  (assign continue (label after-call9))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch11
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call9 ; `val` now contains result of `(factorial (- n 1))`
  (restore argl) ; restore partial argument list for `*`
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc) ; restore `*`
  (restore continue)
  ;; apply `*` and return its value
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch14))
compiled-branch13
;; note that a compound procedure here is called tail-recursively
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch14
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call12
after-if3
after-lambda1
;; assign the procedure to the variable `factorial`
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
))

(compile
  '(define (factorial-alt n)
     (if (= n 1)
       1
       (* n (factorial-alt (- n 1)))))
  'val
  'next)

((env)
 (val)
 (
  (assign val (op make-compiled-procedure) (label entry19) (reg env))
  (goto (label after-lambda18))
entry19
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
  (branch (label primitive-branch34))
compiled-branch33
  (assign continue (label after-call32))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch34
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call32
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch21))
true-branch22
  (assign val (const 1))
  (goto (reg continue))
false-branch21
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const factorial-alt) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch25))
compiled-branch24
  (assign continue (label after-call23))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch25
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call23
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch28))
compiled-branch27
  (assign continue (label after-call26))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch28
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call26
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch31))
compiled-branch30
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch31
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call29
after-if20
after-lambda18
  (perform (op define-variable!) (const factorial-alt) (reg val) (reg env))
  (assign val (const ok))
))

