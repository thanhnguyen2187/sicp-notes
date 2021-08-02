; Exercise 2.14
;
; After considerable work, Alyssa P. Hacker delivers her finished system.
; Several years later, after she has forgotten all about it, she gets a frenzied
; call from an irate user, Lem E. Tweakit. It seems that Lem has noticed the
; formula for parallel resistors can be written in two algebraically equivalent
; ways:
;
; R_1*R_2/(R_1 + R_2)
;
; and
;
; 1/(1/R_1 + 1/R_2)
;
; He has written the following two programs, each of which computes the
; parallel-resistors formula directly:
;
; ...
;
; Lem complains that Alyssa's program gives different answers for the two ways
; of computing. This is a serious complaint.
;
; Demonstrate that Lem is right. Investigate the behavior of the system on a
; variety of arithmetic expressions.
;
; Make some intervals A and B, and use them in computing the expressions A/A and
; A/B. You will get the most insight by using intervals whose width is a small
; percentage of the center value.
;
; Examine the results of the computation in `center-percent` form (see Exercise
; 2.12).

#lang sicp

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (make-center-width c w)
  (make-interval (- c w)
                 (+ c w)))
(define (make-center-percent c p)
  (make-interval (* c (- 1 p))
                 (* c (+ 1 p))))

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))
(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))
(define (percent i)
  (/ (width i)
     (center i)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define A (make-center-percent 8 0.05))
(define B (make-center-percent 9 0.02))
(define 1/A (div-interval (make-interval 1 1)
                          A))
; (center 1/A)
; (percent 1/A)


(add-interval A B)
(mul-interval A B)
(div-interval A B)

(par1 A B)
(par2 A B)

(div-interval A A)

