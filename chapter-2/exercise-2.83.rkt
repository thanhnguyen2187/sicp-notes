; Exercise 2.83
;
; Suppose you are designing a generic arithmetic system for dealing with the
; tower of types shown in Figure 2.25: integer, rational, real, complex. For
; each type (except) complex, design a procedure that raises objects of that
; type one level in the tower. Show how to install a generic `raise` operation
; that will work for each type (except complex).

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
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond ((eq? type1 type2) (apply proc (map contents args)))
                        (t1->t2 (apply-generic-v2 op (t1->t2 a1) a2))
                        (t2->t1 (apply-generic-v2 op a1 (t2->t1 a2)))
                        (else (error "No method for these types "
                                     (list op type-tags))))))
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

  (define (numer x) (car x))
  (define (denum x) (cdr x))

  (define (tag x) (attach-tag 'rational x))

  (define (make a b) (cons a b))
  (define (raise x) ((apply-generic 'make
                                    'real)
                     (/ (numer x) (denum x))))

  (put 'make 'rational
       (tag (lambda (a b) (make a b))))
  (put 'raise 'rational raise)

  'done)

(define (install-real-package)

  (define (make x) (* x 1.0))
  (define (raise x) ((apply-generic 'make 'complex)
                     x 0))

  (put 'make 'real
       (lambda (x) (make x)))
  (put 'raise 'real raise)

  'done)

(define (install-complex-package)

  (define (make a b) (cons a b))

  (put 'make 'complex
       (lambda (a b) (make a b)))

  'done)
