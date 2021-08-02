; Exercise 2.16
;
; Explain, in general, why equivalent algebraic expressions may lead to
; different answers.
;
; Can you devise an `interval-arithmetic` package that does not have this
; shortcoming, or is this task impossible? (Warning: This problem is very
; difficult)
;
; ---
;
; TODO: change the answer
;
; Equivalent algebraic expressions may lead to different answers since the
; representation of any float/real number inside the computer is not precise.
; Computation therefore is not precise either.

#lang sicp

(define (make-interval c p) (cons c p))
(define (center i) (car i))
(define (percent i) (cdr i))

(define (lower-bound i) (* (center i)
                           (- 1 (percent i))))
(define (upper-bound i) (* (center i)
                           (+ 1 (percent i))))

(define (add-interval i1 i2)
  (let ((c1 (center i1))
        (c2 (center i2))
        (p1 (percent i1))
        (p2 (percent i2)))
    (let ((c3 (+ c1 c2)))
      (let ((p3 (/ (+ (* c1 p1)
                      (* c2 p2))
                   c3)))
        (make-interval c3 p3)))))

(define (mul-interval i1 i2)
  (let ((c1 (center i1))
        (c2 (center i2))
        (p1 (percent i1))
        (p2 (percent i2)))
    (let ((c3 (* c1 c2
                 (+ 1 (* p1 p2))))
          (p3 (/ (+ p1 p2)
                 (+ 1 (* p1 p2)))))
      (make-interval c3 p3))))

(define (inv-interval i)
  (make-interval (/ 1 (center i))
                 (percent i)))

(define (div-interval i1 i2)
  (mul-interval i1
                (inv-interval i2)))

; (define (sub-interval x y)
;   (make-interval (- (lower-bound x)
;                     (upper-bound y))
;                  (- (upper-bound x)
;                     (lower-bound y))))

(define (trad i)
  (cons (lower-bound i) (upper-bound i)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 0)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define A (make-interval 8 0.05))
(define B (make-interval 9 0.02))

; (div-interval (make-interval 1 0) A)

(trad (add-interval A B))
(trad (mul-interval A B))
(trad (div-interval A B))

(trad (par1 A B))
(trad (par2 A B))

(trad (div-interval A A))

