; Exercise 3.46
;
; Suppose that we implement `test-and-set!` using an ordinary procedure as shown
; in the text, without attempting to make the operation atomic. Draw a timing
; diagram like the one in Figure 3.29 to demonstrate how the mutex
; implementation can fail by allowing two processes to acquire the mutex at the
; same time.
;
; ---
;
; The steps of `make-mutex`:
;
; 1. Access `(car cell)`
; 2. Check the value
; 3. Set the value
;
; Between the three steps, if we cannot assure that the value within `cell` does
; not change, then the mutex is not guaranteed to work as expected.

#lang sicp

(define (test-and-set! cell)
  (without-interrupts (lambda ()
                        (if (car cell)
                            true
                            (begin (set-car! cell true)
                                   false)))))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire) (if (test-and-set! cell)
                                  (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex)
  (define (clear! cell) (set-car! cell false)))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))
