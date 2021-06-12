#lang sicp

; (define (f n)
;   (cond ((< n 3) n)
;         (else (+ (f (- n 1))
;                  (* 2 (f (- n 2)))
;                  (* 3 (f (- n 3)))))))

(define (f n)
  (define f0 0)
  (define f1 1)
  (define f2 2)
  (define (iter counter n fn1 fn2 fn3)
    (cond ((> counter n) fn3)
          (else (iter (+ counter 1) n
                      fn2 fn3 (+ fn3
                                 (* 2 fn2)
                                 (* 3 fn1))))))
  (cond ((< n 3) n)
        (else (iter 3 n f0 f1 f2))))

(f 3)
(f 4)
(f 5)
(f 6)
(f 7)
(f 8)
(f 9)
(f 10)
(f 11)
