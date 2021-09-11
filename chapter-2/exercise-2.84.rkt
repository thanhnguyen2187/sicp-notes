; Exercise 2.84
;
; Using the `raise` operation of Exercise 2.83, modify the `apply-generic`
; procedure so that it coerces its arguments to have the same type by the method
; of successive raising, as discussed in this section. You will need to devise a
; way to test which of two types is higher in the tower. Do this in a manner
; that is "compatible" with the rest of the system will not lead to problems in
; adding new levels to the tower.

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
