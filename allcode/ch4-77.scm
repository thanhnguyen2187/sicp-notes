; In Section 4.4.3 we saw that `not` and `lisp-value` can cause the query
; language to give "wrong" answers if these filtering operations are applied
; frames in which variables are unbound. Devise a way to fix this shortcoming.
; One idea is to perform the filtering in a "delayed" manner by appending to the
; frame a "promise" to filter that is fulfilled only when enough variables have
; been bound to make the operation possible. We could wait to perform filtering
; until all other operations have been performed. However, for efficiency's
; sake, we would like to perform filtering as soon as possible so as to cut down
; on the number of intermediate frames generated.

(load "ch4-query.scm")

(initialize-data-base microshaft-data-base)

(query-driver-loop)

(define (negate operands frame-stream)
  ; (stream-append (negated-query operands))
  ; (stream-flatmap
  ;   (lambda (frame)
  ;     (if (stream-null? (qeval (negated-query operands)
  ;                              (singleton-stream frame)))
  ;       (singleton-stream frame)
  ;       the-empty-stream))
  ;   (qeval (negated-query operands) frame-stream))
  ; (stream-flatmap
  ;   (lambda (frame)
  ;     (if (stream-null? (qeval (negated-query operands)
  ;                              (singleton-stream frame)))
  ;       (singleton-stream frame)
  ;       the-empty-stream))
  ;   frame-stream)
  ; frame-stream
  )

(supervisor ?x ?y)

(and (supervisor ?x ?y)
     (not (job ?x (computer programmer))))

(and (not (job ?x (computer programmer)))
     (supervisor ?x ?y))

(and (supervisor ?y ?x)
     (not (same (warbucks oliver) ?y)))

(and (not (same (warbucks oliver) ?y))
     (supervisor ?y ?x))

