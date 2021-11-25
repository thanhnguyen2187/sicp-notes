; Exercise 3.59
;
; In Section 2.5.3 we saw how to implement a polynomial arithmetic system
; representing polynomials as lists of terms. In a similar way, we can work with
; *power series*, such as:
;
; ...
;
; represented as infinite streams. We will represent the series ... as the
; stream whose elements are the coefficients a_0, a_1, a_2, ...
;
; a. The integral of the series a_0 + a_1 x + a_2 x^2 + a_3 x^3 + ... is the
; series
;
; ...
;
; where c is the constant.
;
; Define a procedure `integrate-series` that takes as input a stream a_0, a_1,
; ... representing a power series and returns the stream a_0, 1/2 a_1, 1/3 a_2,
; ... of coefficients of the non-constant terms of the integral of the series.
; (Since the result has no constant term, it doesn't represent a power series;
; when we use `integrate-series`, we will `cons` on the appropriate constant.)
;
; b. The function x |-> e^x is its own derivative. This implies that e^x and the
; integral of e^x are the same series, except for the constant term, which is
; e^0 = 1. Accordingly, we can generate the series for e^x as
;
; ...
;
; Show how to generate the series for sine and cosine, starting from the facts
; that the derivative of sine is cosine and the derivative of cosine is the
; negative of sine:
;
; ...

#lang racket

(require racket/stream)

(define (stream-maps f . streams)
  (if (ormap stream-empty? streams)
      empty-stream
      (let ((first-elements (map stream-first streams)))
        (stream-cons (apply f
                            first-elements)
                     (apply stream-maps
                            f
                            (map stream-rest streams))))))

(define (stream-add . streams)
  (apply stream-maps + streams))

(define (stream-mul . streams)
  (apply stream-maps * streams))

(define (stream-div . streams)
  (apply stream-maps / streams))

(define (integers-start-from n)
  (stream-cons n (integers-start-from (+ n 1))))

(define integers (integers-start-from 1))

(define (make-stream . elements)
  (if (empty? elements)
      empty-stream
      (stream-cons (first elements)
                   (apply make-stream (rest elements)))))

#| (define (integrate-series A) |#
#|   (stream-rest ())) |#

(define (inc x) (+ x 1))

#| (stream->list (stream-take (stream-maps * integers integers) 3)) |#

(define (integrate-series S)
  (stream-rest (stream-div S integers)))

(stream->list (integrate-series (make-stream 1 2 3 4 5)))

(define (make-infinite-stream stream)
  (stream-append stream (make-infinite-stream stream)))

(define series-1-0--1-0
  (make-infinite-stream (make-stream 1 0 -1 0)))

#| (define series-0--1-1 |#
#|   (stream-cons 0 (stream-cons -1 (stream-cons -1 series-1-0--1)))) |#

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

;; (stream->list (stream-take exp-series 10))

(stream->list (stream-take series-1-0--1-0 10))

