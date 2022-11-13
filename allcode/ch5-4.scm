; Specify register machines that implement each of the following procedures. For
; each machine, write a controller instruction sequence and draw a diagram
; showing the data paths.
;
; a. Recursive exponentiation:
;
; ...
;
; b. Iterative exponentiation:
;
; ...

;; a.

(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(controller

  (assign b (op read))
  (assign n (op read))
  (assign continue (label expt-done))

 expt-loop
  (test (op =) (reg n) (const 0))
  (branch (label expt-base-case))
  (save n)
  (save continue)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label expt-after))
  (goto (label expt-loop))

 expt-after
  (restore continue)
  (restore n)
  (assign val (op *) (reg b) (reg val))
  (goto (reg continue))

 expt-base-case
  (assign val (const 1))
  (goto (reg continue))

 expt-done)

;; b.

(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
      product
      (expt-iter (- counter 1)
                 (* b product)))))

(controller
  (assign b (op read))
  (assign n (op read))
  (assign val (const 1))

 expt-iter
  (test (op =) (const 0) (reg n))
  (branch (label expt-done))
  (assign val (op *) (reg val) (reg b))
  (assign n (op -) (reg n) (const 1))
  (goto (label expt-iter))

 expt-done)

