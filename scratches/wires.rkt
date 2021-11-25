; Exercise ?.?
;
; A wire in our simulation will be a computational object with two local state
; variables:
; 
; - a `signal-value` (initially taken to be 0) and
; - a collection of `action-procedures` to be run when the signal changes value.
;
; We implement the wire, using message-passing style, as a collection of local
; procedures together with a `dispatch` procedure that selects the appropriate
; local operation, just as we did with the simple bank-account object in Section
; 3.1.1.

#lang sicp

;; QUEUE

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

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))
(define (front-queue queue)
  (queue 'front-queue))
(define (delete-queue! queue item)
  ((queue 'delete-queue!) item))

;; AGENDA

; Time Segment
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time  s) (car s))
(define (segment-queue s) (cdr s))

; Agenda
(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))
(define (rest-segment  agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)

  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))

  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              (cdr segments)))
              (add-to-segments! rest)))))

  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda (cons (make-new-time-segment time action)
                                    segments))
        (add-to-segments! segments)))

  )

(define (removing-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (emtpy-queue? q)
        (set-segments! agenda
                       (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define the-agenda (make-agenda))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (emtpy-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (removing-first-agenda-item! the-agenda)
        (propagate))))

;; WIRE
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))

    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures)))

    (define (dispatch message)
      (cond ((eq? message 'get-signal) signal-value)
            ((eq? message 'set-signal!) set-my-signal!)
            ((eq? message 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" message))))

    dispatch))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (probe name wire)
  (add-action! wire (lambda ()
                      (display name) (display " ")
                      (display (current-time the-agenda))
                      (display "  New-value = ")
                      (display (get-signal wire))
                      (newline))))

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 count)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1)
                                 (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
