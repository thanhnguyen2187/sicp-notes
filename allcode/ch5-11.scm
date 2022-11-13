; When we introduce `save` and `restore` in Section 5.1.4, we didn't specify
; what would happen if you tried to restore a register that was not the last one
; saved, as in the sequence
;
;   (save y) (save x) (restore y)
;
; There are several reasonable possibilities for the meaning of `restore`:
;
; a. `(restore y)` puts into `y` the last value saved from the stack, regardless
; of what register that value came from. This is the way our simulator behaves.
; Show how to take advantage of this behavior to eliminate one instruction from
; the Fibonacci machine of Section 5.1.4 (Figure 5.12)
;
; b. `(restore y)` puts into `y` the last value saved on the stack, but only if
; that value was saved from `y`; otherwise, it signals an error. Modify the
; simulator to behave this way. You will have to change `save` to put the
; register name on the stack along with the value.
;
; c. `(restore y)` puts into `y` the last value saved from `y` regardless of
; what other registers were saved after `y` and not restored. Modify the
; simulator to behave this way. You will have to associate a separate stack with
; each register. You should make the `initialize-stack` operation initialize all
; the register stacks.

(load "ch5-regsim.scm")

; b.

(define (make-stack-element reg-name value)
  (list reg-name value))

(define (stack-element-reg-name stack-element)
  (car stack-element))

(define (stack-element-value stack-element)
  (cadr stack-element))

(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    (lambda ()
      (push stack (make-stack-element reg-name (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    (lambda ()
      (let* ((stack-element (pop stack))
             (popped-reg-name (stack-element-reg-name stack-element))
             (popped-value (stack-element-value stack-element)))
        (if (eq? popped-reg-name reg-name)
          (set-contents! reg popped-value)
          (error "Trying to restore pushed value of register" popped-reg-name
                 "to register" reg-name "-- MAKE-RESTORE"))
        (advance-pc pc))
      )))

(define stack-machine
  (make-machine
    '(x y)
    (list
      (list 'display display))
    '(
      (assign x (const 1))
      (save x)
      (assign y (const 2))
      (restore x)
      done)))

(start stack-machine)

; c. TODO

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(list-ref (list 1 2 3 4) 2)

(string<? (symbol->string 'a))

(define (add-sorted sequence element)
  (if (null? sequence)
    (list element)
    (let ((first-element (car sequence)))
      (cond ((= first-element element) sequence)
            ((> first-element element) (cons element sequence))
            (else (cons first-element
                        (add-sorted (cdr sequence) element)))))))

(add-sorted (list 1 2 3) 0)

(add-sorted (list 1) 2)

