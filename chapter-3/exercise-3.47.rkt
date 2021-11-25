; Exercise 3.47
;
; A semaphore (of size *n*) is a generalization of a mutex. Like a mutex, a
; semaphore supports acquire and release operations, but it is more general in
; that up to *n* processes can acquire it concurrently. Additional processes
; that attempt to acquire the semaphore must wait for release operations. Give
; implementations of semaphores
;
; a. in terms of mutexes
; b. in terms of atomic `test-and-set!` operations.

#lang sicp

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

(define (make-semaphore n)
  ; Like a mutex, semaphore supports acquire and release operations, but it is
  ; more general in that up to *n* processes can acquire it concurrently.
  ; Additional processes that attempt to acquire the semaphore must wait for
  ; release operations.

  (let ((current-counter 0))
      (define (iterate . ps)
        (if (> current-counter n)))

      iterate)
  )

(define semaphore (make-semaphore 3))


