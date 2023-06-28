; - What order of evaluation does our compiler produce for operands of a
;   combination? Is it left-to-right, right-to-left, or some other order?
; - Where in the compiler is this order determined? Modify the compiler so that
;   it produces some other order of evaluation. (See the discussion of order of
;   evaluation for the explicit-control evaluator in Section 5.4.1.)
; - How does changing the order of operand evaluation affect the efficiency of
;   the code that constructs the argument list?
;
; ---
;
; The compiler produces operands right-to-left, which is determined by
; `construct-arglist` and `code-to-get-rest-args`. The generated order can be
; changed by removing `reverse` at `construct-arglist`, but at
; `construct-arglist`, an instruction to reverse `argl` is needed. There should
; not be a big performance penalty, since all we do is to move `reverse` from
; one place (in the "built-in" `construct-arglist`) to another (a direct
; instruction).

(load "ch5-compiler.scm")

(define label-counter 14)

(define (construct-arglist operand-codes)
  ; (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
      (make-instruction-sequence '() '(argl)
                                 '((assign argl (const ()))))
      (let ((code-to-get-last-arg
              (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                                           '((assign argl (op list) (reg val)))))))
        (if (null? (cdr operand-codes))
          code-to-get-last-arg
          (preserving '(env)
                      code-to-get-last-arg
                      (code-to-get-rest-args (cdr operand-codes))))))
    ; )
  )

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
          (preserving '(argl)
                      (car operand-codes)
                      (make-instruction-sequence '(val argl) '(argl)
                                                 '((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
      (append-instruction-sequences
        code-for-next-arg
        (make-instruction-sequence '(argl) '(argl)
                                   '((assign argl (op reverse) (reg argl)))))
      (preserving '(env)
                  code-for-next-arg
                  (code-to-get-rest-args (cdr operand-codes))))))

(compile
  '(define (f x)
     (+ x (g (+ x 2))))
  'val
  'next)

((env)
 (val)
 (
  (assign val (op make-compiled-procedure) (label entry16) (reg env))
  (goto (label after-lambda15))
entry16
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const g) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op list) (reg val))
  (assign val (const 2))
  (assign argl (op cons) (reg val) (reg argl))
  (assign argl (op reverse) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch19))
compiled-branch18
  (assign continue (label after-call17))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch19
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call17
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch22))
compiled-branch21
  (assign continue (label after-call20))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch22
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call20
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (assign argl (op reverse) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch25))
compiled-branch24
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch25
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call23
after-lambda15
  (perform (op define-variable!) (const f) (reg val) (reg env))
  (assign val (const ok))))
