; The simulator can be used to help determine the data paths required for
; implementing a machine with a given controller. Extend the assembler to store
; the following information in the machine model:
;
; - a list of all instructions, with duplicates removed, sorted by instruction
; type (`assign`, `goto`, and so on);
; - a list (without duplicates) of the registers used to hold entry points
; (these are the registers referenced by `goto` instructions)
; - a list (without duplicates) of the registers that are `saved` or `restored`
; - for each register, a list (without duplicates) of the sources from which it
; is assigned (for example, the sources for register `val` in the factorial
; machine of Figure 5.11 are `(const 1)` and `((op *) (reg n) (reg val))`)
;
; Extend the message-passing interface to the machine to provide access to this
; new information. To test your analyzer, define the Fibonacci machine from
; Figure 5.12 and examine the lists you constructed.

(load "ch5-regsim.scm")

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
            (list (list 'pc pc) (list 'flag flag)))
          (unique-op-names (list 'initialize-stack
                                 'print-stack-statistics)))
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
      ;; 5-12
      (define inst-names '())
      (define entry-point-registers '())
      (define saved-restored-registers '())
      (define register-sources '())
      (define (add-unique sequence item)
        (if (null? sequence)
          (list item)
          (let* ((first-item     (car sequence))
                 (rest-sequence  (cdr sequence))
                 (item-str       (symbol->string item))
                 (first-item-str (symbol->string first-item)))
            (cond ((string=? first-item-str item-str) sequence)
                  ((string>? first-item-str item-str) (cons item sequence))
                  (else (cons first-item
                              (add-unique rest-sequence item))))
            )))
      ;
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; 5-12 part 1
              ((eq? message 'set-inst-names!)
               (lambda (new-inst-names)
                 (map (lambda (inst-name)
                        (set! inst-names (add-unique inst-names inst-name)))
                      new-inst-names)))
              ((eq? message 'inst-names) inst-names)
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; 5-12 part 2
              ((eq? message 'set-entry-point-registers!)
               (lambda (register-names)
                 (map (lambda (register-name)
                        (set!
                          entry-point-registers
                          (add-unique entry-point-registers register-name)))
                      register-names)))
              ((eq? message 'entry-point-registers) entry-point-registers)
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; 5-12 part 3
              ((eq? message 'saved-restored-registers) saved-restored-registers)
              ((eq? message 'set-saved-restored-registers!)
               (lambda (register-names)
                 (map (lambda (register-name)
                        (set!
                          saved-restored-registers
                          (add-unique saved-restored-registers register-name)))
                      register-names)))
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; 5-12 part 4
              ((eq? message 'set-register-sources!)
               (lambda (register-sources)
                 (map (lambda (register-source)
                        (let* ((register-name (car register-source))
                               (source-value (cdr register-source))))))))
              ((eq? message 'register-sources) register-sources)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 5-12 part 1
    ((machine 'set-inst-names!)
     (map inst-name controller-text))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 5-12 part 2
    (let* ((goto-insts (filter goto? controller-text))
           (registers (map register-name-in-goto goto-insts)))   
      ((machine 'set-entry-point-registers!) registers))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 5-12 part 3
    (let* ((save-or-restore-insts
             (filter (lambda (inst)
                       (or (save? inst)
                           (restore? inst)))
                     controller-text))
           (register-names (map register-name-in-save-or-restore
                                save-or-restore-insts)))   
      ((machine 'set-saved-restored-registers!) register-names))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 5-12 part 4
    machine))

(define (inst-name inst)
  (if (pair? inst)
    (car inst)
    'label))

(define (goto? inst)
  (and (pair? inst)
       (eq? (car inst) 'goto)))

(define (save? inst)
  (and (pair? inst)
       (eq? (car inst) 'save)))

(define (restore? inst)
  (and (pair? inst)
       (eq? (car inst) 'restore)))

(define (register-name-in-save-or-restore inst)
  (cadr inst))

(define (register-name-in-goto inst)
  (cadadr inst))

(define fib-machine
  (make-machine
    '(n continue val)
    (list
      (list '< <)
      (list '- <)
      (list '+ <)
      (list 'read read)
      )
    '(
      (assign n (op read))
      (assign continue (label fib-done))
      fib-loop
      (test (op <) (reg n) (const 2))
      (branch (label immediate-answer))
      ;; set up to compute Fib(n-1)
      (save continue)
      (assign continue (label afterfib-n-1))
      (save n)                           ; save old value of n
      (assign n (op -) (reg n) (const 1)); clobber n to n-1
      (goto (label fib-loop))            ; perform recursive call
      afterfib-n-1                         ; upon return, val contains Fib(n-1)
      (restore n)
      (restore continue)
      ;; set up to compute Fib(n-2)
      (assign n (op -) (reg n) (const 2))
      (save continue)
      (assign continue (label afterfib-n-2))
      (save val)                         ; save Fib(n-1)
      (goto (label fib-loop))
      afterfib-n-2                         ; upon return, val contains Fib(n-2)
      (assign n (reg val))               ; n now contains Fib(n-2)
      (restore val)                      ; val now contains Fib(n-1)
      (restore continue)
      (assign val                        ; Fib(n-1)+Fib(n-2)
              (op +) (reg val) (reg n)) 
      (goto (reg continue))              ; return to caller, answer is in val
      immediate-answer
      (assign val (reg n))               ; base case: Fib(n)=n
      (goto (reg continue))
      fib-done)))

(fib-machine 'inst-names)

(fib-machine 'entry-point-registers)

(fib-machine 'saved-restored-registers)

(cadadr (list 1 (list 2 3)))

(assq 'ab '((a 1) (b 2) (c 3)))

(equal? (list 1 2) (list 1 2))

