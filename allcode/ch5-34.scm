; Compile the interactive factorial procedure
;
; ...
;
; Annotate the resulting code, showing the essential difference between the code
; for iterative and recursive versions of factorial that makes one process build
; up stack space and the other run in constant space.
;
; ---
;
; ...

(load "ch5-compiler.scm")

(compile
  '(define (factorial n)
     (define (iter product counter)
       (if (> counter n)
         product
         (iter (* counter product)
               (+ counter 1))))
     (iter 1 1))
  'val
  'next)

((env)
 (val)
 (
  (assign val (op make-compiled-procedure) (label entry36) (reg env))
  (goto (label after-lambda35))
entry36 ;; entry to `factorial`
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (assign val (op make-compiled-procedure) (label entry41) (reg env))
  (goto (label after-lambda40))
entry41 ;; entry to `iter`
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const >) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch56))
compiled-branch55
  (assign continue (label after-call54))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch56
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call54 ;; done with checking `(> counter n)`
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch43))
true-branch44 ; return `product`
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue))
false-branch43 ; recursive calling
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (save continue)
  (save proc)
  (save env)
  ;; preparation for `(+ counter 1)`
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  ;; 
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch50))
compiled-branch49
  (assign continue (label after-call48))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch50
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call48
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  ;; preparation for `(* counter product)`
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  ;;
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch47))
compiled-branch46
  (assign continue (label after-call45))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch47
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call45
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  ;; arg should have `(* counter product)` and `(+ counter 1)` ready
  (restore proc) ; `proc` now contains `iter`
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch53))
compiled-branch52
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch53
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call51
after-if42
after-lambda40
  ;; after evaluating `iter`'s body
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  ;; invoking `(iter 1 1)`
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  ;;
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch39))
compiled-branch38
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch39 ; return from `(iter 1 1)`
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call37
after-lambda35
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
)



