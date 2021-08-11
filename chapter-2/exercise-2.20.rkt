; Exercise 2.20
;
; The procedure +, *, and `list` take arbitrary numbers of arguments. One way to
; define such procedures is to use `define` with /dotted-tail notation/. In a
; procedure definition, a parameter list that has dot before the last parameter
; name indicates that:
;
; - when the procedure is called,
; - the initial parameters (if any) will have as values the initial arguments,
; as usual,
; - but the final parameter's value will be a /list/ of any remaining arguments.
; 
; ...
;
; Use this notation to write a procedure `same-parity` that takes one or more
; integers and returns a list of all the arguments that have the same even-odd
; parity as the first argument.

#lang sicp

(define (same-parity a . l)
  (define (iterate l result)
    (let ((b (car l))
          (z (cdr l)))
      (if (null? z)
          result
          (iterate (cdr l)
                   (append result
                           (if (= (remainder a 2)
                                  (remainder b 2))
                               (list b)
                               (list)))))))
  (iterate l
           (list a)))

(same-parity 2 4 3 5 6 7)
