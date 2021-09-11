; Exercise 2.86
;
; Suppose we want to handle complex numbers whose real parts, imaginary parts,
; magnitudes, and angles can be either ordinary numbers, rational numbers, or
; other numbers we might wish to add to the system.
;
; Describe and implement the changes to the system needed to accomodate this.
; You will have to define operations such as `sine` and `cosine` that are
; generic over ordinary numbers and rational numbers.
;
; ---
;
; The changes needed are: usage of `get` within complex number package.

#lang sicp

(define (square) 0)

(define (put) 0)
(define (get) 0)

(define (get-coercion) 0)
(define (put-coercion) 0)

(define (attach-tag type-tag contents)
  (cons (type-tag contents)))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG")))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS")))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC " (list op type-tags))))))

(define (tower-index type-tag)
  (cond ((eq? type-tag 'integer)  1)
        ((eq? type-tag 'rational) 2)
        ((eq? type-tag 'real)     3)
        ((eq? type-tag 'complex)  4)
        (else (error "Invalid type: TOWER-INDEX " type-tag))
        ))

(define (direct-lower type-tag)
  (cond ;; ((eq? type-tag 'integer)  1)
        ((eq? type-tag 'rational) 'integer)
        ((eq? type-tag 'real)     'rational)
        ((eq? type-tag 'complex)  'real)
        (else (error "Invalid type: DIRECT-LOWER " type-tag))))

(define (lower? type-1 type-2)
  (< (tower-index type-1)
     (tower-index type-2)))

(define (apply-generic-v2 op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car  type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car  args))
                    (a2 (cadr args)))
                (cond ((lower? type1 type2) (apply-generic-v2 'raise a1)
                                            a2)
                      ((lower? type2 type1) a1
                                            (apply-generic-v2 'raise a2))
                      (error "No method for these types "
                             (list op type-tags))))
              (error "No method for these types "
                     (list op type-tags)))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-integer-package)

  (define (make x) x)
  (define (raise x) ((apply-generic 'make 'rational)
                     (cons x 1)))

  (put 'make 'integer (lambda (x) (make x)))
  (put 'raise 'integer raise)

  'done)

(define (install-rational-package)

  (define (make a b) (cons a b))
  (define (numer x) (car x))
  (define (denum x) (cdr x))
  (define (equal? x y)
    (= (* (numer x) (denum y))
       (* (denum x) (numer y))))

  (define (tag x) (attach-tag 'rational x))

  (define (raise x) ((apply-generic 'make 'real)
                     (/ (numer x) (denum x))))
  (define (project x) ((apply-generic 'project 'integer)
                       (quotient (numer x) (denum x))))
  (define (drop x)
    (let ((projected (project x)))
      (let ((projected-raised ((apply-generic 'raise 'integer) projected)))
            (if (equal? x projected-raised)
                projected
                x))))

  (put 'make 'rational
       (tag (lambda (a b) (make a b))))
  (put 'raise 'rational raise)
  (put 'drop 'rational drop)

  'done)

(define (fractionalize) 0)

(define (install-real-package)

  (define (make x) (* x 1.0))
  (define (raise x) (apply (apply-generic 'make 'complex)
                           x 1))
  (define (project x) (apply (apply-generic 'make 'rational)
                             (fractionalize x)))
  (define (drop x)
    (let ((projected (project x)))
      (let ((projected-raised ((apply-generic 'raise 'rational) projected)))
            (if (equal? x projected-raised)
                projected
                x))))

  (put 'make 'real
       (lambda (x) (make x)))
  (put 'raise 'real raise)
  (put 'drop 'real drop)

  'done)

(define (install-complex-package)

  (define (make a b) (cons a b))

  (define (real x) (car x))
  (define (imag x) (cdr x))

  (define (project x) ((apply-generic 'make 'real)
                       (real x)))
  (define (drop x)
    (let ((projected (project x)))
      (let ((projected-raised ((apply-generic 'raise 'real) projected)))
            (if (equal? x projected-raised)
                projected
                x))))

  (put 'make 'complex
       (lambda (a b) (make a b)))

  'done)
