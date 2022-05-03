; Ben Bitdiddle tests the lazy list implementation given above by evaluating the
; expression
;
;   (car '(a b c))
;
; To his surprise, this produce an error. After some thought, he realizes that
; the "lists" obtained by reading in quoted expressions are different from the
; lists maintained by the new definitions of `cons`, `car`, and `cdr`. Modify
; the evaluator's treatment of quoted expressions so that quoted lists typed at
; the driver loop will produce true lazy lists.

(load "ch4-leval.scm")

(define primitive-procedures
  (list ; ()
        ; (list 'car (lambda (p q) p))
        ; (list 'cdr (lambda (p q) q))
        ; (list 'cons (lambda (m) (m x y)))
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'newline newline)
        (list 'display display)
;;      more primitives
        ))

(RESTART 1)

(let* ((env (setup-environment))
       (local-eval (lambda (exp) (eval exp env))))

  (local-eval '(define (cons x y) (lambda (m) (m x y))))
  (local-eval '(define (car z) (lambda (p q) p)))
  (local-eval '(define (cdr z) (lambda (p q) q)))

  (local-eval '(define p (cons 1 2)))
  (local-eval '(display p))

  ; (local-eval '(define (list-ref items n)
  ;                (if (= n 0)
  ;                  (car items)
  ;                  (list-ref (cdr items)
  ;                            (- n 1)))))
  ; (local-eval '(define (map proc items)
  ;                (if (null? items)
  ;                  '()
  ;                  (cons (proc (car items))
  ;                        (map proc (cdr items))))))
  ; (local-eval '(define (scale-list items factor)
  ;                (map (lambda (x) (* x factor))
  ;                     items)))
  ; (local-eval '(define (add-lists list1 list2)
  ;                (cond ((null? list1) list2)
  ;                      ((null? list2) list1)
  ;                      (else (cons (+ (car list1) (car list2))
  ;                                  (add-lists (cdr list1)
  ;                                             (cdr list2)))))))

  ; (local-eval '(define ones (cons 1 1)))
  ; (local-eval '(define interger (cons 1 (add-lists ones integers))))


  ; (local-eval '(display (list-ref integers 17)))
  ; (local-eval '(display (list-ref ones 1)))
  ; (local-eval '(display (list-ref ones 99)))
  )
