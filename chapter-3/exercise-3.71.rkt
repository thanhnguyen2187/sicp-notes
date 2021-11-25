; Exercise 3.71
;
; Numbers that can be expressed as the sum of two cubes in more than one way are
; sometimes called Ramanujan numbers, in honor of the mathematician Srinivasa
; Ramanujan.
;
; Ordered streams of paris provide an elegant solution to the problem of
; computing these numbers. To find a number that can be written as the sum of
; two cubes in two different ways, we need only
;
; - Generate the stream of pairs of integers *(i, j)* weighted according to the
; sum *i^3 + j^3* (see Exercise 3.70),
; - Then search the stream for two consecutive pairs with the same weight.
;
; Write a procedure to generate the Ramanujan numbers. The first such number is
; 1,729. What are the next five?

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

(define (cube x) (* x x x))

(define (add-sum-cube pair)
  (append pair
          (list (sum-cube pair))))

;; TODO: use cache
(define (sum-cube pair)
  (+ (cube (car pair))
     (cube (cadr pair))))

(define P (stream-map add-sum-cube
                      (pairs integers integers)))
(define P-2
  (stream-map add-sum-cube
              (pairs-weighted (lambda (pair)
                                (sum-cube pair))
                              integers
                              integers)))

(define (make-ramanujan-filter counter)
  ;; Create a Ramanujan filter which can be used within "stream-filter" of
  ;; triples (a, b, a^3 + b^3) to get Ramanujan numbers. Use two local
  ;; variables "current-number", and "current-counter" to track the filtered
  ;; numbers and act accordingly. "counter" is 2 for the "normal" cases (numbers
  ;; which are represented by two different a and b pairs).
  (let ((current-number -1)
        (current-counter 0))
    (lambda (triple)
      (let ((number (caddr triple)))
        (if (= number current-number)
            (begin (set! current-counter (inc current-counter))
                   (if (= counter current-counter)
                   (begin (set! current-number -1)
                          (set! current-counter 0)
                          #t)
                   #f))
            (begin (set! current-number number)
                   (set! current-counter 1)
                   #f)))
      ))
  )

(define ramanujan-filter (make-ramanujan-filter 2))

(define P-3
  (stream-filter ramanujan-filter P-2))

(take-stream P-3 5)

(define ramanujan-filter-2 (make-ramanujan-filter 3))

(define P-4
  (stream-filter ramanujan-filter-2 P-2))

;; (take-stream P-4 5)
