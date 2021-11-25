; Exercise 3.50
;
; Complete the following definition, which generalizes `stream-map` to allow
; procedures that take multiple arguments, analogous to `map` in Section 2.2.1,
; Footnote 12.

#lang sicp

(define (cons-stream a b)
  (cons a (delay b)))

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

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map
                          (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
  (display x) (newline))

(define (display-stream s)
  (stream-for-each display-line s))

(define S1 (stream-enumerate-interval 1 11))
(define S2 (stream-enumerate-interval 2 12))

(display-stream (stream-map + S1 S2))

