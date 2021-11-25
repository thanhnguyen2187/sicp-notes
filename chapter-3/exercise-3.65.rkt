; Exercise 3.65
;
; Use the series
;
; ...
;
; to compute three sequences of approximations to the natural logarithm of 2, in
; the same way we did above for pi. How rapidly do these sequences converge?

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

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
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

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 
                             (* -2 s1)
                             s2)))
                 (euler-transform (stream-cdr s)))))

(define (ln-2-summands start)
  (cons-stream (/ 1.0 start)
               (mul-streams minus-ones
                            (ln-2-summands (+ start
                                              1)))))

(define ln-2-stream
  (partial-sums (ln-2-summands 1.0)))

(define ln-2-stream-transformed
  (euler-transform ln-2-stream))

(take-stream ln-2-stream-transformed 10)

