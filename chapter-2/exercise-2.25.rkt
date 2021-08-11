; Exercise 2.25
;
; Give combinations of `car`s and `cdr`s that will pick 7 from each of the
; following lists:
;
; (1 3 (5 7) 9)
; ((7))
; (1 (2 (3 (4 (5 (6 7))))))

#lang sicp

(define L1 (list 1 3 (list 5 7) 9))
(define L2 (list (list 7)))
(define L3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(define (chain-apply fs xs)
  (if (null? fs)
      xs
      (chain-apply (cdr fs)
                   ((car fs) xs))))

(chain-apply (list cdr
                   cdr
                   car
                   cdr
                   car) L1)
(chain-apply (list car
                   car) L2)
(chain-apply (list cdr car
                   cdr car
                   cdr car
                   cdr car
                   cdr car
                   cdr car) L3)
