; Modify the simulator so that it uses the controller sequence to determine what
; registers the machine has rather than requiring a list of registers as an
; argument to `make-machine`. Instead of pre-allocating the registers in
; `make-machine`, you can allocate them one at a time when they are first seen
; during assembly of the instructions.

(load "ch5-regsim.scm")

;; should existed built-in procedures

(define (filter-map pred func seq)
  (map func
       (filter pred seq)))

(define (map-filter func pred seq)
  (filter pred
          (map func seq)))

;; utility procedures

(define (inst-name inst)
  (if (pair? inst)
    (car inst)
    'label))

(define (goto? inst)
  (equal? (inst-name inst) 'goto))

(define (save? inst)
  (equal? (inst-name inst) 'save))

(define (assign? inst)
  (equal? (inst-name inst) 'assign))

(define (restore? inst)
  (equal? (inst-name inst) 'restore))

(define (register-name-in-save inst)
  (cadr inst))

(define (register-name-in-restore inst)
  (cadr inst))

(define (register-name-in-goto inst)
  (cadadr inst))

(define (register-name-in-assign inst)
  (cadr inst))

;; main procedures

(define (extract-register-names controller-text)
  (let* ((relevant-insts (map-filter
                           (lambda (inst)
                             (cond ((save? inst) (register-name-in-save inst))
                                   ((restore? inst) (register-name-in-restore inst))
                                   ((goto? inst) (register-name-in-goto inst))
                                   ((assign? inst) (register-name-in-assign inst))
                                   (else '())))
                           (lambda (register-name)
                             (not (null? register-name)))
                           controller-text))
         (relevant-insts-set (make-equal-hash-table)))
    (for-each (lambda (inst)
                (hash-table-set! relevant-insts-set inst #t))
              relevant-insts)
    (hash-table-keys relevant-insts-set)))

(define (make-machine-5-13 ops controller-text)
  (let ((machine (make-new-machine))
        (register-names (extract-register-names controller-text)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; testing

(define fib-machine-controller-text
  '(;; hard code n's value for faster testing
    ; (assign n (op read))
    (assign n (const 10))
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
    afterfib-n-1                       ; upon return, val contains Fib(n-1)
    (restore n)
    (restore continue)
    ;; set up to compute Fib(n-2)
    (assign n (op -) (reg n) (const 2))
    (save continue)
    (assign continue (label afterfib-n-2))
    (save val)                         ; save Fib(n-1)
    (goto (label fib-loop))
    afterfib-n-2                       ; upon return, val contains Fib(n-2)
    (assign n (reg val))               ; n now contains Fib(n-2)
    (restore val)                      ; val now contains Fib(n-1)
    (restore continue)
    (assign val                        ; Fib(n-1)+Fib(n-2)
            (op +) (reg val) (reg n)) 
    (goto (reg continue))              ; return to caller, answer is in val
    immediate-answer
    (assign val (reg n))               ; base case: Fib(n)=n
    (goto (reg continue))
    fib-done))

(let* ((operators (list (list '< <)
                        (list '- -)
                        (list '+ +)
                        (list 'read read)))
       (fib-machine (make-machine-5-13 operators
                                       fib-machine-controller-text)))
  (start fib-machine)
  (get-register-contents fib-machine 'val)) ; should be 55

