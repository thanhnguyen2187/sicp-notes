; Exercise 3.12 of Section 3.3.1 presented an `append` procedure that appends
; two lists to form a new list and `append!` procedure that splices two lists
; together. Design a register machine to implement each of these procedures.
; Assume that the list-structure memory operations are available as primitive
; operations.
;
; ---
;
; ...

(load "ch5-regsim.scm") ; or load stuff from ch5-19

; sample code

(define (my-append x y)
  (if (null? x)
    y
    (cons (car x)
          (my-append (cdr x) y))))

(define (last-pair l)
  (cond ((null? l) (error "LAST-PAIR received empty list"))
        ((not (pair? l)) (error "LAST-PAIR received literal value " l))
        ((null? (cdr l)) l)
        (else (last-pair (cdr l)))))

(define (my-append! x y)
  (set-cdr! (last-pair x) y)
  x)

(let ((x '(1 2 3 4))
      (y '(5 6 7 8)))
  (set-cdr! x 2))

; testing

(my-append '(1 2 3 4) '(5 6 7 8))

(my-append! '(1 2 3 4) '(5 6 7 8))

; register machine

; `append`

(let* ((controller-text
         '(controller
             (assign continue (label done))
             ; (perform (op read) (reg lst-1))
             ; (perform (op read) (reg lst-2))
           recursion
             (test (op null?) (reg lst-1))
             (branch (label base-case))
             ;; set up before recursive calling
             (save continue)
             (save lst-1)
             (assign continue (label after-recursion))
             (assign lst-1 (op cdr) (reg lst-1))
             (goto (label recursion))
           after-recursion
             (restore lst-1)
             (restore continue)
             ;; 
             (assign aux (op car) (reg lst-1))
             (assign lst-2 (op cons) (reg aux) (reg lst-2))
             (goto (reg continue))
           base-case
             ; in base case, the result is stored at lst-2
             (goto (reg continue))
           done))
       (ops (list
              (list 'display display)
              (list 'read read)
              (list 'null? null?)
              (list 'car car)
              (list 'cdr cdr)
              (list 'cons cons)
              ))
       (registers '(continue lst-1 lst-2 aux))
       (machine (make-machine registers ops controller-text)))
  ; (set-register-contents! machine 'lst-1 '())
  (set-register-contents! machine 'lst-1 '(1 2 3 4))
  (set-register-contents! machine 'lst-2 '(5 6 7 8))
  (machine 'trace-on!)
  ; ((machine 'reg-tracing-on!) 'val)
  (start machine)
  (get-register-contents machine 'lst-2))

; append!

(let* ((controller-text
         '(controller
           main
             (assign continue (label main-done))
             (assign aux (reg lst-1))
             (test (op null?) (reg lst-1))
             (branch (label main-alt))
             (goto (label last-pair))
           main-done
             (perform (op set-cdr!) (reg aux) (reg lst-2))
             (goto (label done))
           main-alt
             (assign lst-1 (reg lst-2))
             (goto (label done))
           last-pair
             (assign aux (op cdr) (reg aux))
             (assign aux-2 (op cdr) (reg aux))
             (test (op null?) (reg aux-2))
             (branch (label last-pair-done))
             (goto (label last-pair))
           last-pair-done
             ; result is put in aux
             (goto (reg continue))
           done))
       (ops (list
              (list 'display display)
              (list 'read read)
              (list 'null? null?)
              (list 'set-cdr! set-cdr!)
              (list 'cdr cdr)
              (list 'cddr cddr)
              ))
       (registers '(continue lst-1 lst-2 aux aux-2))
       (machine (make-machine registers ops controller-text)))
  (set-register-contents! machine 'lst-1 '())
  ; (set-register-contents! machine 'lst-1 '(1 2 3 4))
  ; (set-register-contents! machine 'lst-1 '(1 2 3 4))
  (set-register-contents! machine 'lst-2 '(5 6 7 8))
  (machine 'trace-on!)
  ; ((machine 'reg-tracing-on!) 'val)
  (start machine)
  (get-register-contents machine 'lst-1))

