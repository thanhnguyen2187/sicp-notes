; Louis Reasoner wonders why the `simple-query` and `disjoin` procedures
; (Section 4.4.4.2) are implemented using explicit `delay` operations, rather
; than being defined as follows:
;
; ...
;
; Can you give examples of queries where these simpler definitions would lead to
; undesirable behavior?
;
; ---
;
; `simple-query` and `disjoin` are implemented using explicit `delay`, rather
; than straight `stream-append`, since `apply-rules` within `stream-append` is a
; "normal" function that has applicative order, and may create unwanted
; behavior.
;
; In some infinite loop cases, `delay` may produce some result, while the
; suggested way may not:

;; original
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

;; EXERCISE 4.71
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append (find-assertions query-pattern frame)
                    (apply-rules query-pattern frame)))
   frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
       (qeval (first-disjunct disjuncts) frame-stream)
       (disjoin (rest-disjuncts disjuncts) frame-stream))))

;;

(assert! (tmp1 a))
(assert! (rule (tmp1 ?x) (tmp1 ?x)))

;; with delay

(tmp1 a)
(tmp1 a)
(tmp1 a)
(tmp1 a)
(tmp1 a)

;; without delay

;

