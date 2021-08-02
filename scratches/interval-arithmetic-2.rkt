;

#lang sicp

(define (make-interval-center-percent c p)
  (cons (cons (* c (- 1 p))
              (* c (+ 1 p)))
        (cons c
              p)))
(define (make-interval-lower-upper l u)
  (cons (cons l
              u)
        (let ((c (/ (+ u l)
                    2)))
          (let ((w (- u l)))
            (let ((p (/ (/ w 2)
                        c)))
              (cons c
                    p))))))

(define (lower-bound i)
  (car (car i)))
(define (upper-bound i)
  (cdr (car i)))
(define (center i)
  (car (cdr i)))
(define (percent i)
  (cdr (cdr i)))

(define (add-interval i1 i2)
  (make-interval-lower-upper (+ (lower-bound i1) (lower-bound i2))
                             (+ (upper-bound i1) (upper-bound i2))))
; (define (mul-interval i1 i2)
;   (let ((p1 (* (lower-bound i1) (lower-bound i2)))
;         (p2 (* (lower-bound i1) (upper-bound i2)))
;         (p3 (* (upper-bound i1) (lower-bound i2)))
;         (p4 (* (upper-bound i1) (upper-bound i2))))
;     (make-interval-lower-upper (min p1 p2 p3 p4)
;                                (max p1 p2 p3 p4))))
(define (mul-interval i1 i2)
  (let ((c1 (center i1))
        (c2 (center i2))
        (p1 (percent i1))
        (p2 (percent i2)))
    (let ((c3 (* c1 c2
                 (+ 1 (* p1 p2))))
          (p3 (/ (+ p1 p2)
                 (+ 1 (* p1 p2)))))
      (make-interval-center-percent c3 p3))))

(define (inv-interval i)
  (make-interval-center-percent (/ 1 (center i)) (percent i)))

(define (div-interval i1 i2)
  (mul-interval i1 (inv-interval i2)))

(define (trad i)
  (car i))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval-center-percent 1 0)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; (define A (make-interval-center-percent 8 0.05))
; (define B (make-interval-center-percent 9 0.02))

(define A (make-interval-lower-upper 2 8))
(define B (make-interval-lower-upper 2 8))

(trad (add-interval A B))
(trad (mul-interval A B))
(trad (div-interval A B))

(trad (par1 A B))
(trad (par2 A B))

(trad (div-interval A A))
