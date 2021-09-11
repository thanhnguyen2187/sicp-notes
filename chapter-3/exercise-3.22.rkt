; Exercise 3.22
;
; Instead of representing a queue as a pair of pointers, we can build a queue as
; a procedure with local state. The local state will consist of pointers to the
; beginning and the end of an ordinary list. Thus, the `make-queue` procedure
; will have the form
;
; ...
;
; Complete the definition of `make-queue` and provide implementations of the
; queue opreations using this representation.

#lang sicp

(define ... 0)

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (empty-queue?)
      (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue" (cons front-ptr rear-ptr))
          (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (if (empty-queue?)
            (begin (set! front-ptr new-pair)
                   (set! rear-ptr  new-pair))
            (begin (set-cdr! rear-ptr new-pair)
                   (set! rear-ptr new-pair)))))

    (define (delete-queue!)
      (if (empty-queue?)
          (error "DELETE! called with an empty queue" (cons front-ptr rear-ptr))
          (set! front-ptr (cdr front-ptr))))

    (define (print-queue)
      (display front-ptr) (newline))

    (define (dispatch message)
      (cond ((eq? message 'insert-queue!) insert-queue!)
            ((eq? message 'delete-queue!) delete-queue!)
            ((eq? message 'print-queue)   print-queue)
            ((eq? message 'front-queue)   front-queue)
            ((eq? message 'empty-queue?)  empty-queue?)
            (else "Equivalent procedure not found for message MAKE-QUEUE " message)))

    dispatch))

(define q1 (make-queue))
; ((q1 'delete-queue!))
((q1 'insert-queue!) 'a)
((q1 'insert-queue!) 'b)
((q1 'insert-queue!) 'c)
((q1 'print-queue))
((q1 'delete-queue!))
((q1 'delete-queue!))
((q1 'print-queue))
((q1 'front-queue))
