; Our implementation of `and` as a series of combination of queries (Figure 4.5)
; is elegant, but it is inefficient because in processing the second query of
; the `and` we must scan the data base for each frame produced by the first
; query. If the data base has `n` elements, and a typical query produces a
; number of out put frames proportional to *n* (say *n / k*), then scanning the
; data base for each frame produced by the first query will require *n^2 / k*
; calls to the pattern matcher. Another approach would be to process the two
; clauses of the `and` separately, then look for all pairs of output frames that
; are compatible. If each query produces *n / k* output frames, then this means
; that we must perform *n^2 / k^2* compatibility checks -- a factor of *k* fewer
; than the number of matches required in our current method.
;
; Devise an implementation of `and` that uses this strategy. You must implement
; a procedure that takes two frames as inputs, checks whether the binding of the
; frames are compatible, and, if so, produces a frame that merges the two sets
; of bindings. This operation is similar to unification.

(load "ch4-query.scm")

(define (stream-zip stream-1 stream-2)
  (if (or (stream-null? stream-1)
          (stream-null? stream-2))
    the-empty-stream
    (cons-stream (list (stream-car stream-1)
                       (stream-car stream-2))
                 (stream-zip (stream-cdr stream-1)
                             (stream-cdr stream-2)))))

(define (stream-cartesian-product stream-1 stream-2)
  (if (or (stream-null? stream-1)
          (stream-null? stream-2))
    the-empty-stream
    (cons-stream (list (stream-car stream-1)
                       (stream-car stream-2))
                 (stream-cartesian-product stream-2
                                           (stream-cdr stream-1)))))

(define ones (cons-stream 1 ones))

(define (stream-add stream-1 stream-2)
  (stream-map
    (lambda (pair) (+ (car pair)
                      (cadr pair)))
    (stream-zip stream-1 stream-2)))

(define (stream-take stream n)
  (if (= n 0)
    the-empty-stream
    (cons-stream (stream-car stream)
                 (stream-take (stream-cdr stream)
                              (- n 1)))))

(define integers
  (cons-stream 0
               (stream-add ones integers)))

(display-stream (stream-take integers 10))

(display-stream (stream-take (stream-cartesian-product integers integers) 20))

(define (failed? frame) (eq? 'failed frame))
(define (succeeded? frame) (not (failed? frame)))
;
(define empty-frame? null?)
(define first-binding car)
(define rest-bindings cdr)
;
(define (merge-frame-streams frame-stream-1
                             frame-stream-2)
  ; (stream-filter
  ;   succeeded?
  ;   (stream-flatmap
  ;     (lambda (frame-pair)
  ;       (let ((frame-1 (car frame-pair))
  ;             (frame-2 (cadr frame-pair)))
  ;         (merge-frames frame-1 frame-2)))
  ;     (stream-cartesian-product frame-stream-1 frame-stream-2)))
  (stream-flatmap
    (lambda (frame-1)
      (stream-filter succeeded?
                     (stream-map
                       (lambda (frame-2)
                         (merge-frames frame-1 frame-2))
                       frame-stream-2)))
    frame-stream-1))
;
(define (merge-frames frame-1 frame-2)
  (cond ((or (failed? frame-1)
             (failed? frame-2))
         'failed)
        ((empty-frame? frame-1)
         frame-2)
        (else
          (let* ((binding (first-binding frame-1))
                 (variable (binding-variable binding))
                 (value (binding-value binding))
                 (frame-2-extended (extend-if-possible variable value frame-2)))
            (if (failed? frame-2-extended)
              'failed
              (merge-frames (rest-bindings frame-1) frame-2-extended))))))

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
    frame-stream
    (merge-frame-streams (conjoin (rest-conjuncts conjuncts) frame-stream)
                         (qeval (first-conjunct conjuncts)
                                frame-stream))))

(initialize-data-base 'microshaft-data-base)

(query-driver-loop)

(and (supervisor ?x ?y)
     (supervisor ?y ?v))

(supervisor ?x ?y)

(outranked-by ?person ?who)

V

(display-stream frame-stream)

A

J

0
