; Exercise 2.11
;
; In passing, Ben also cryptically comments: "By testing the signs of the
; endpoints of the intervals, it is possible to break `mul-interval` into nine
; cases, only one of which requires more than two multiplications."
;
; Rewrite this procedure using Ben's suggestion.

#lang sicp

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
(define ... 0)

(define (mul-interval-optimized x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (let ((x-neg (< x2 0))
          (x-pos (> x1 0))
          (y-neg (< y2 0))
          (y-pos (> y1 0)))
      (cond (x-neg (cond (y-neg (make-interval (* x2 y2) (* x1 y1)))
                         (y-pos (make-interval (* x1 y2) (* x2 y1)))
                         (else  (make-interval (* x1 y2) (* x1 y1)))))
            (x-pos (cond (y-neg (make-interval (* x2 y1) (* x1 y2)))
                         (y-pos (make-interval (* x1 y1) (* x2 y2)))
                         (else  (make-interval (* x2 y1) (* x2 y2)))))
            (else  (cond (y-neg (make-interval (* x2 y1) (* x1 y1)))
                         (y-pos (make-interval (* x1 y2) (* x2 y2)))
                         (else  (make-interval (min (* x1 y2) (* x2 y1))
                                               (max (* x1 y1) (* x2 y2))))))))))

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0)
           (>= (upper-bound y) 0))
      (error "ZERO WIDTH INTERVAL")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

