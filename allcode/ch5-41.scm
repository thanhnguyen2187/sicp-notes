; Write a procedure `find-variable` that takes as arguments a variable and a
; compile-time environment and returns the lexical address of the variable with
; respect to that environment. For example, in the program fragment that is
; shown above, the compile-time environment during the compilation of expression
; `<e1>` is `((y z) (a b c d e) (x y))`. `find-variable` should produce
;
; ...

(define (make-lexical-address frame-number displacement-number)
  (list frame-number displacement-number))

(define (find-variable variable compile-time-env)
  (define (find-in-frame variable frame)
    (define (iterate index variable frame)
      (cond ((null? frame) 'not-found)
            ((eq? variable (car frame)) index)
            (else (iterate (+ index 1) variable (cdr frame)))))
    (iterate 0 variable frame))
  (define (iterate frame-number
                   variable
                   compile-time-env)
    (if (null? compile-time-env)
      'not-found
      (let* ((frame (car compile-time-env))
             (found-displacement-number (find-in-frame variable frame)))
        (if (eq? found-displacement-number 'not-found)
          (iterate (+ frame-number 1)
                   variable
                   (cdr compile-time-env))
          (make-lexical-address frame-number found-displacement-number)))))
  (iterate 0 variable compile-time-env))

(find-variable 'c '((y z) (a b c d e) (x y)))
; (1 2)

(find-variable 'x '((y z) (a b c d e) (x y)))
; (2 0)

(find-variable 'w '((y z) (a b c d e) (x y)))
; not-found
