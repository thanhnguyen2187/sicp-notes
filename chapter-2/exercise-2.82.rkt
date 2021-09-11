; Exercise 2.82
;
; Show how to generalize `apply-generic` to handle coercion in the general case
; of multiple arguments. One strategy is to attempt to coerce all the arguments to
; the type of the first argument, then to the type of the second argument, and
; so on. Give an example of a situation where this strategy (and likewise the
; two-argument) version given above is not sufficiently general.
;
; (Hint: Consider the case where there are some suitable mixed-type operations
; present in the table that will not be tried.)

#lang sicp

; natural number -> rational -> real -> complex
; natural + complex + real

; A -> B -> C
;      B -> D
;
; C + D

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
          (if (>= (length args) 2)
              (let ((type1 (car  type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car  args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (if (= (length args) 2)
                    (cond ((eq? type1 type2) (apply proc (map contents args)))
                          (t1->t2 (apply-generic op (t1->t2 a1) a2))
                          (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                          (else (error "No method for these types "
                                       (list op type-tags))))
                    (cond ((eq? type1 type2)
                           (apply-generic op (list (apply proc (map contents args))
                                                   (cddr args))))
                          (t1->t2
                            (apply-generic op (list (t1->t2 a1)
                                                    a2
                                                    (cddr args))))
                          (t2->t1
                            (apply-generic op (list a1
                                                    (t2->t1 a2)
                                                    (cddr args))))
                          (else (error "No method for these types "
                                       (list op type-tags)))))))
              (error "No method for these types "
                     (list op type-tags)))))))

