; Design a machine to compute square roots using Newton's method, as described
; in Section 1.1.7:
;
; ...
;
; Begin by assuming that `good-enough?` and `improve` operations are available
; as primitives. Then show how to expand these in terms of arithmetic
; operations. Describe each version of the `sqrt` machine design by drawing a
; data-path diagram and writing a controller definition in the register-machine
; language.

;; version 1: `good-enough?` and `improve` are available as primitives

(controller
  (assign g (const 1))
  test-g
  (test (op good-enough?) (reg g))
  (branch (label sqrt-done))
  (assign g (op improve) g)
  (goto (label test-g))
  sqrt-done)

;; version 2:

(controller
  (assign g (const 1))
  (assign x (op read))
  test-g
  (assign gs (op square) (reg g))
  (assign d  (op -)      gs x)
  (assign da (op abs)    d)
  (test (op <) (reg da) (const 0.001))
  (branch (label sqrt-done))
  (assign q (op /) x g)
  (assign a (op average) g q)
  (assign g (reg a))
  (goto (label test-g))
  sqrt-done)


