; Exercise 1.25
;
; Alyssa P. Hacker complains that we went to a lot of extra work in writing
; `expmod`. After all, she says, since we already know how to compute
; exponentials, we could have simply written
;
; ...
;
; Is she correct? Would this procedure serve as well for our fast prime tester?
; Explain.

#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (square n) (* n n))

; (define (fast-expt b n)
;   (cond ((= n 0) 1)
;         ((even? n) (square (fast-expt b (/ n 2))))
;         (else (* b (fast-expt b (- n 1))))))

(define (fast-expt b n)
  (define (iter b n)
    (cond ((= n 0) 1)
          ((= n 1) b)
          ((even? n) (iter (square b)
                           (/ n 2)))
          (else (iter (* b (square b))
                      (/ 2 (- n 1))))))
  (iter b n))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))
