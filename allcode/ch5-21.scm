; Implement register machines for the following procedures. Assume that the
; list-structure memory operations are available as machine primitives.
;
; a. Recursive `count-leaves`:
;
;   ...
;
; b. Recursive `count-leaves` with explicit counter:
;
;   ...

(load "ch5-regsim.scm") ; or load stuff from ch5-19

;; a.

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(let* ((controller-text
        '(controller
            (assign tree (op read))
            (assign continue (label done))
          recursion
            (test (op null?) (reg tree))
            (branch (label base-case-0))
            (test (op not-pair?) (reg tree))
            (branch (label base-case-1))
            ;; setup to compute first recursive procedure
            (save continue)
            (assign continue (label after-recursion-1))
            (save tree)
            (assign tree (op car) (reg tree))
            (goto (label recursion))
          after-recursion-1
            (restore tree)
            (restore continue)
            ;; setup to compute second recursive procedure
            (assign tree (op cdr) (reg tree))
            (save continue)
            (assign continue (label after-recursion-2))
            (save val)
            (goto (label recursion))
          after-recursion-2
            (assign tree (reg val)) ; store recursive call 1's result to `tree`
            (restore val) ; store recursive call 2's result to `val`
            (restore continue)
            (assign val
                    (op +) (reg val) (reg tree))
            (goto (reg continue))
          base-case-0
            (assign val (const 0))
            (goto (reg continue))
          base-case-1
            (assign val (const 1))
            (goto (reg continue))
          done))
       (ops
         (list
           (list 'null? null?)
           (list 'not-pair? (lambda (val) (not (pair? val))))
           (list 'read read)
           (list 'display display)
           (list 'newline newline)
           (list 'car car)
           (list 'cdr cdr)
           (list '+ +)
           ))
       (registers '(tree continue val))
       (machine (make-machine registers ops controller-text)))
  (machine 'trace-on!)
  ((machine 'reg-tracing-on!) 'val)
  (start machine)
  (get-register-contents machine 'val))

;; b.

(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else
            (count-iter (cdr tree)
                        (count-iter (car tree)
                                    n)))))
  (count-iter tree 0))

(let* ((controller-text
        '(controller
            (assign n (const 0))
            ; (assign tree (op read))
            (assign continue (label done))
          iterate
            (test (op null?) (reg tree))
            (branch (label base-case-0))
            (test (op not-pair?) (reg tree))
            (branch (label base-case-1))
            ;; set up for two inner calls
            (save continue)
            (save tree)
            (save continue)
            (save tree)
            (assign continue (label after-iterate-1))
            (assign tree (op cdr) (reg tree))
            ;;
            (goto (label iterate))
          after-iterate-1
            ;; set up for second call
            (restore tree)
            (restore continue)
            (assign tree (op car) (reg tree))
            (assign continue (label after-iterate-2))
            (goto (label iterate)) ; do recursive call
          after-iterate-2 ; final call is done
            (restore tree)
            (restore continue)
            (goto (reg continue))
          base-case-0
            (goto (reg continue))
          base-case-1
            (assign n (op +) (reg n) (const 1))
            (goto (reg continue))
          done))
       (ops
         (list
           (list 'null? null?)
           (list 'not-pair? (lambda (val) (not (pair? val))))
           (list 'read read)
           (list 'display display)
           (list 'newline newline)
           (list 'car car)
           (list 'cdr cdr)
           (list '+ +)
           ))
       (registers '(tree continue n val))
       (machine (make-machine registers ops controller-text)))
  (machine 'trace-on!)
  ((machine 'reg-tracing-on!) 'n)
  ; quick setting for easier testing
  (set-register-contents! machine 'tree '(1 2 ((3) 4) (5) (6 (7 8))))
  (start machine)
  (get-register-contents machine 'n))

