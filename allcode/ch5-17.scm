; Extend the instruction tracing of Exercise 5.16 so that before printing an
; instruction, the simulator prints any labels that immediately precede that
; instruction in the controller sequence. Be careful to do this in a way that
; does not interfere with instruction counting (Exercise 5.15). You will have to
; make the simulator retain the necessary label information.
;
; ---
;
; The exercise is quite challenging, as putting a `current-label` inside
; `make-new-machine` can handle the case of executing `goto (label ...)`, since
; extracting the label in this case is easy. It is not the same with `goto (reg
; ...)`, as the content of a register in this case is a list of instructions
; instead of the real label.
;
; I could not find a better way than modifying the instructions' structure
;
; - Old version: `(<raw text> . <procedure>)`, for example `((goto (reg
; continue))
; - New version: `(<label> <raw text> <procedure>)`
;
; and the functions that use `inst`:
;
; - `make-instruction`
; - `instruction-text`
; - `instruction-execution-proc`
; - `set-instruction-execution-proc!`

(load "ch5-regsim.scm")

;; instruction structure procedures

(define (make-instruction label text)
  (list label text '()))

(define (instruction-label inst)
  (list-ref inst 0))

(define (instruction-text inst)
  (list-ref inst 1))

(define (instruction-execution-proc inst)
  (list-ref inst 2))

(define (set-instruction-execution-proc! inst proc)
  (list-set! inst 2 proc))

(define (assemble controller-text machine)
  (extract-labels
    '*unassigned*
    controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))

(define (extract-labels current-label
                        text
                        receive)
  (if (null? text)
      (receive '() '())
      (let* ((next-inst (car text))
             (next-inst-label? (symbol? next-inst))
             (label (if next-inst-label?
                      next-inst
                      current-label)))
        (extract-labels
          label
          (cdr text)
          (lambda (insts labels)
            (if next-inst-label?
              (receive insts
                       (cons (make-label-entry label insts)
                             labels))
              (receive (cons (make-instruction label next-inst)
                             insts)
                       labels)))))))

;; main procedures

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        ;; Exercise 5.15
        (instruction-count 0)
        ;; Exercise 5.16
        (trace #f)
        )
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
              ;; Exercise 5.16
              (let* ((inst (car insts))
                     (inst-label (instruction-label inst))
                     (inst-text (instruction-text inst))
                     (inst-proc (instruction-execution-proc inst)))
                ;; Exercise 5.15
                (set! instruction-count (+ 1 instruction-count))
                ;; Exercise 5.17
                (if trace
                  (begin
                    (display "Label: ")
                    (display inst-label)
                    (display "; ")
                    (display "Executing instruction: ")
                    (display inst-text)
                    (newline)))
                (inst-proc)
                (execute)))))
              ;;
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
              ;; Exercise 5.15
              ((eq? message 'get-and-reset-instruction-count)
               (newline)
               (display "Instruction count: ") (display instruction-count)
               (set! instruction-count 0))
              ;; Exercise 5.16
              ((eq? message 'trace-on!) (set! trace #t))
              ((eq? message 'trace-off!) (set! trace #f))
              ((eq? message 'trace-on?) trace)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;; testing

;;FIGURE 5.11
(define fact-controller-text
  '(controller
      (assign n (op read))
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
    fact-done))

(define fact-ops
  (list
    (list '= =)
    (list '- -)
    (list '* *)
    (list 'read read)))

(define fact-registers
  '(n continue val))

(let ((fact-machine (make-machine
                      fact-registers
                      fact-ops
                      fact-controller-text)))
  (fact-machine 'trace-on!)
  (start fact-machine)
  )
