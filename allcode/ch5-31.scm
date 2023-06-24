; In evaluating a procedure application, the explicit-control evaluator always:
;
; - Saves and restores the `env` register around the evaluation of the operator,
; - Saves and restores `env` around the evaluation of each operand (except the
; final one),
; - Saves and restores `argl` around the evaluation of each operand, and
; - Saves and restores `proc` around the evaluation of the operand sequence.
;
; For each of the following combinations, say which of these `save` and
; `restore` operations are superfluous and thus could be eliminated by the
; compiler's `preserving` mechanism:
;
;   (f 'x 'y)
;   ((f) 'x 'y)
;   (f (g 'x) y)
;   (f (g 'x) 'y)
;
; ---
;
; The table summarize if the `save` and `restore` operations for each is needed.
; "x" indicates yes, while a blank space indicates no.
;
; |                 | 1. `env` | 2. `env` | 3. `argl` | 4. `proc` |
; | --------------- | -------- | -------- | --------- | --------- |
; | `(f 'x 'y)`     |          |          |           |           |
; | `((f) 'x 'y)`   |          |          |           |           |
; | `(f (g 'x) y)`  |          |    x     |     x     |     x     |
; | `(f (g 'x) 'y)` |          |          |     x     |     x     |
; 

; relevant sections
ev-application
  (save continue)
  (save env) ; 1.
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))
ev-appl-did-operator
  (restore unev)
  (restore env) ; 1.
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc) ; 4.
ev-appl-operand-loop
  (save argl) ; 3.
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env) ; 2.
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))
ev-appl-accumulate-arg
  (restore unev)
  (restore env) ; 2.
  (restore argl) ; 3.
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))
ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  (restore argl) ; 3.
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc) ; 4.
  (goto (label apply-dispatch))
apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))  
  (branch (label compound-apply))
  (goto (label unknown-procedure-type))

