; Exercise 3.23
;
; A _deque_ ("double-ended queue") is a sentence in which items can be inserted
; and deleted at either the front or the rear.
;
; Operations on deques are the
; - constructor `make-deque`,
; - the predicate `empty-deque?`,
; - selectors `front-deque` and `rear-deque`,
; - mutators `front-insert-deque!`, `rear-insert-deque!`, `front-delete-deque!`,
; and `rear-delete-deque!`
;
; Show how to represent deques using pairs, and give implementation of the
; operations. All operation should be accomplished in O(1) steps.

#lang sicp

(define (make-element)

  (let ((prev-ptr nil)
        (value    nil)
        (next-ptr nil))

      (define (dispatch message)
        (cond ((eq? message 'prev-ptr) prev-ptr)
              ((eq? message 'value) value)
              ((eq? message 'next-ptr) next-ptr)
              ((eq? message 'set-prev-ptr!) (lambda (item) (set! prev-ptr item)))
              ((eq? message 'set-value!)    (lambda (item) (set! value    item)))
              ((eq? message 'set-next-ptr!) (lambda (item) (set! next-ptr item)))
              ((eq? message 'set!) (lambda (new-prev-ptr new-value new-next-ptr)
                                     (begin (set! prev-ptr new-prev-ptr)
                                            (set! value    new-value)
                                            (set! next-ptr new-next-ptr))))
              (else (error "Invalid message MAKE-ELEMENT" message))))

    dispatch))

(define (construct-element front-ptr
                           value
                           rear-ptr)
  (let ((new-element (make-element)))

    ((new-element 'set!) front-ptr
                         value
                         rear-ptr)
    new-element))

(define (make-deque)

  (let ((front-ptr nil)
        (rear-ptr  nil))

    (define (empty-deque?)
      (or (null? front-ptr)
          (null? rear-ptr)))

    (define (front-deque)
      (if (empty-deque?)
          (error "FRONT called with an empty deque " (cons front-ptr rear-ptr))
          (front-ptr 'value)))

    (define (insert-deque! message item)
      (let ((new-element (construct-element nil item nil)))

        (cond ((empty-deque?)       (begin (set! front-ptr new-element)
                                           (set! rear-ptr  new-element)))
              ((eq? message 'front) (begin ((new-element   'set-next-ptr!) front-ptr)
                                           ((front-ptr     'set-prev-ptr!) new-element)
                                           (set! front-ptr new-element)))
              ((eq? message 'rear)  (begin ((new-element   'set-prev-ptr!) rear-ptr)
                                           ((rear-ptr      'set-next-ptr!) new-element)
                                           (set! rear-ptr  new-element)))
              (else (error "Invalid message INSERT-DEQUE! " message)))))

    (define (delete-deque! message)
      (cond ((empty-deque?) (error "DELETE-DEQUE! called with an empty deque "
                                   (cons front-ptr rear-ptr)))
            ((eq? front-ptr rear-ptr) (begin
                                        ; special case where the deque only has one element
                                        (set! front-ptr nil)
                                        (set! rear-ptr  nil)))
            ((eq? message 'front) (begin
                                    ; "move" the pointers:
                                    ; - `front-ptr` is moved to the next of itself
                                    ; - New `front-ptr`'s `prev` is turned to nil
                                    (set! front-ptr (front-ptr 'next-ptr))
                                    ((front-ptr 'set-prev-ptr!) nil)))
            ((eq? message 'rear)  (begin
                                    ; "move" the pointers:
                                    ; - `rear-ptr` is moved to the previous of itself
                                    ; - New `rear-ptr`'s `next` is turned into nil
                                    (set! rear-ptr (rear-ptr 'prev-ptr))
                                    ((rear-ptr 'set-next-ptr!) nil)))
            (else (error "Invalid message DELETE-DEQUE! " message))))

    (define (print-deque ptr)
      (if (not (null? ptr))
          (begin (display (ptr 'value)) (display " ")
                 (print-deque (ptr 'next-ptr)))
          (newline)))

    (define (dispatch message)
      (cond ((eq? message 'empty-deque?)        empty-deque?)
            ((eq? message 'front-insert-deque!) (lambda (item)
                                                  (insert-deque! 'front item)))
            ((eq? message 'rear-insert-deque!)  (lambda (item)
                                                  (insert-deque! 'rear item)))
            ((eq? message 'front-delete-deque!) (lambda ()
                                                  (delete-deque! 'front)))
            ((eq? message 'rear-delete-deque!)  (lambda ()
                                                  (delete-deque! 'rear)))
            ((eq? message 'print-deque)         (lambda ()
                                                  (print-deque front-ptr)))
            (else (error "Invalid message MAKE-QUEUE" message))))

    dispatch))

(define dq (make-deque))

((dq 'front-insert-deque!) 1)
; 0
((dq 'rear-insert-deque!)  2)
; 1 2
((dq 'rear-insert-deque!)  3)
; 1 2 3
((dq 'front-insert-deque!) 0)
; 0 1 2 3
((dq 'rear-insert-deque!) 0)
; 0 1 2 3 0
((dq 'front-insert-deque!) 0)
; 0 0 1 2 3 0

((dq 'front-delete-deque!))
; 0 1 2 3 0
((dq 'rear-delete-deque!))
; 0 1 2 3

((dq 'print-deque))

((dq 'front-delete-deque!))
; 1 2 3

((dq 'print-deque))
