;

#lang sicp

(define (sum term
             lower
             next
             upper)
  (if (> lower upper) 0
      (+ (term lower)
         (sum term
              (next lower)
              next
              upper))))

(define (sum-normal lower
                    upper)
  (sum identity
       lower
       inc
       upper))

(define (cube x) (* x x x))

(define (sum-cubes lower
                   upper)
  (sum cube
       lower
       inc
       upper))

(define (pi-sum lower upper)
  (define (pi-term x)
    (/ 1.0
       (* x
          (+ x
             2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term
       lower
       pi-next
       upper))

; (sum-normal 1 20)
; (sum-cubes 1 10)

(define (integral f
                  a
                  b
                  dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f
          (+ a
             (/ dx 2.0))
          add-dx
          b)
     dx))

(integral cube 0 1 0.001)
(integral cube 0 1 0.0001)
