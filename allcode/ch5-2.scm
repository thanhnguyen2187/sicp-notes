; User the register-machine language to describe the iterative factorial machine
; of Exercise 5.1

(controller
  (assign p (const 1))
  (assign c (const 1))
  test-c
  (test (op <) (reg n) (reg c))
  (branch (label factorial-done))
  (assign r (op *) (reg p) (reg c))
  (assign p r)
  (assign r (op +) (reg c) (const 1))
  (assign c r)
  (goto (label test-c))
  factorial-done)
