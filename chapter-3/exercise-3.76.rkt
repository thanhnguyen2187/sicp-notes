; Exercise 3.76
;
; Eva Lu Ator has criticism of Louis's approach in Exercise 3.75. The program he
; wrote is not modular, because it intermixes the operation of smoothing with
; the zero-crossing extraction.
;
; For example, the extractor should not have to be changed if Alyssa finds a
; better way to condition her input signal.
;
; Help Louis by writing a procedure `smooth` that takes a stream as input and
; produces a stream in which each element is the average of two successive input
; stream elements. Then use `smooth` as a component to implement the
; zero-crossing detector in a more modular style.

#lang sicp

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a d) (cons a (delay d)))))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-null? stream)
  (null? stream))

(define the-empty-stream '())

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s)
                  (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (ormap pred lst)
  (if (null? lst)
      #f
      (or (pred (car lst))
          (ormap pred (cdr lst)))))

(define (stream-map proc . argstreams)
  (if (ormap stream-null? argstreams)
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map
                          (cons proc (map stream-cdr argstreams))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (display-line x)
  (display x) (newline))

(define (display-stream s)
  (stream-for-each display-line s))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers
  (integers-starting-from 1))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (partial-sums stream)
  (add-streams stream
               (cons-stream 0 (partial-sums stream))))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream s1car
                                (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream s2car
                                (merge (stream-cdr s2) s1)))
                  (else (cons-stream s1car
                                     (merge (stream-cdr s1)
                                            (stream-cdr s2)))))))))

(define fibs
  (cons-stream
    0
    (cons-stream 1
                 (add-streams (stream-cdr fibs)
                              fibs))))

(define (take-stream stream count)
  (if (or (= count 0)
          (stream-null? stream))
      nil
      (cons (stream-car stream)
            (take-stream (stream-cdr stream) (- count 1)))))

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den)
            den radix)))

(define minus-ones
  (cons-stream -1 minus-ones))

(define (stream-limit stream tolerance)
  (let ((rest-stream (stream-cdr stream)))
    (let ((first-element (stream-car stream))
          (second-element (stream-car rest-stream)))
      (if (<= (abs (- first-element
                      second-element))
              tolerance)
          first-element
          (stream-limit rest-stream tolerance)))))

#| (define (interleave s1 s2) |#
#|   (if (stream-null? s1) |#
#|       s2 |#
#|       (cons-stream (stream-car s1) |#
#|                    (interleave s2 (stream-cdr s1))))) |#

(define (interleave . S)
  (if (null? S)
      the-empty-stream
      (let ((s1 (stream-car S)))
        (if (stream-null? s1)
            (apply interleave (cdr S))
            (cons-stream (stream-car s1)
                         (apply interleave (append (cdr S)
                                                   (list (stream-cdr s1))))))))
  )

(define (merge-weighted W s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< (W s1car)
                      (W s2car))
                   (cons-stream s1car
                                (merge-weighted W
                                                (stream-cdr s1)
                                                s2)))
                  ((> (W s1car)
                      (W s2car))
                   (cons-stream s2car
                                (merge-weighted W
                                                (stream-cdr s2)
                                                s1)))
                  (else (cons-stream s1car
                                     (cons-stream s2car
                                                  (merge-weighted W
                                                                  (stream-cdr s1)
                                                                  (stream-cdr s2)))
                                     )))))))

(define (make-stream . elements)
  (if (null? elements)
      the-empty-stream
      (cons-stream (car elements)
                   (apply make-stream (cdr elements)))))

(define (make-pair a b)
  (list a b))

(define (make-triple a b c)
  (list a b c))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s)
                                    x))
                  (stream-cdr t))
      (pairs (stream-cdr s)
             (stream-cdr t)))))

(define (pairs-weighted W s t)
  (cons-stream
    (make-pair (stream-car s)
               (stream-car t))
    (merge-weighted
      W
      (stream-map (lambda (x) (make-pair (stream-car s)
                                         x))
                  (stream-cdr t))
      (pairs-weighted W
                      (stream-cdr s)
                      (stream-cdr t)))))

(define (sign-change-detector s1 s2)
  (cond ((and (> 0 s1) (< 0 s2)) -1)
        ((and (< 0 s1) (> 0 s2)) 1)
        (else 0)))

(define (average a b)
  (/ (+ a b)
     2))

(define (smooth S)
  (stream-map average
              S
              (stream-cdr S)))

(define sense-data
  (make-stream 1 2 1.5
               1 0.5 -0.1
               -2 -3 -2
               -0.5 0.2 3
               4))

(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0
                           sense-data)))

(define smoothed-zero-crossings
  (let ((smoothed-sense-data (smooth sense-data)))
    (stream-map sign-change-detector
                smoothed-sense-data
                (cons-stream 0
                             smoothed-sense-data)))
  )

(take-stream (smooth sense-data) 16)
(take-stream zero-crossings 16)
(take-stream smoothed-zero-crossings 16)
