; Exercise ?.?

#lang sicp

;;;SECTION 3.5

;; (define (stream-map proc . argstreams)
;;   (if (<??> (car argstreams))
;;       the-empty-stream
;;       (<??>
;;        (apply proc (map <??> argstreams))
;;        (apply stream-map
;; 	      (cons proc (map <??> argstreams))))))

(define (accumulate combiner
                    null-value
                    term
                    a
                    next
                    b)
  (if [> a b]
    null-value
    (combiner (term a)
              (accumulate combiner
                          null-value
                          term
                          (next a)
                          next
                          b))))

(define (flatmap procedure
                 sequence)
  (accumulate append
              nil
              (map procedure
                   sequence)))

(define (filter predicate
                sequence)
  (flatmap (lambda (element)
             (if (predicate element)
                 (list element)
                 nil))
           '()
           sequence))

(define (cons-stream a b)
  (cons a (delay b)))

;;;SECTION 3.5.1

;; (define (sum-primes a b)
;;   (define (iter count accum)
;;     (cond ((> count b) accum)
;;           ((prime? count) (iter (+ count 1) (+ count accum)))
;;           (else (iter (+ count 1) accum))))
;;   (iter a 0))


(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

;: (car (cdr (filter prime?
;:                   (enumerate-interval 10000 1000000))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))



;; stream-car and stream-cdr would normally be built into
;;  the stream implementation
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(stream-car
 (stream-cdr
  (stream-filter prime?
                 (stream-enumerate-interval 10000 1000000))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


;; force would normally be built into
;;  the stream implementation
;: (define (force delayed-object)
;:   (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))
