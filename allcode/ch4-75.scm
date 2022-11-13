; Implement for the query language a new special form called `unique`. `unique`
; should succeed if there is precisely one term in the data base satisfying a
; specified query. For example,
;
; ...
;
; should print the one-item stream
;
; ...
;
; since Ben is the only computer wizard, and
;
; ...
;
; should print the empty stream, since there is more than one computer
; programmer. Moreover,
;
; ...
;
; should list all the jobs that are filled by only one person, and the people
; who fill them.
;
; There are two parts to implementing `unique`. The first is to write a
; procedure that handles this special form, and the second is to make `qeval`
; dispatch to that procedure. The second part is trivial, since `qeval` does its
; dispatching in a data-directed way. If your procedure is called
; `uniquely-asserted`, all you need to do is
;
; ...
;
; and `qeval` will dispatch to this procedure for every query whose `type`
; `(car)` is the symbol `unique`.
;
; The real problem is to write the procedure `uniquely-asserted`. This should
; take as input the `contents` (`cdr`) of the unique query, together with a
; stream of frames. For each frame in the stream, it should use `qeval` to find
; the stream of all extensions to the frame that satisfy the given query. Any
; stream that does not have exactly one item in it should be eliminated. The
; remaining streams should be passed back to be accumulated into one big stream
; that is the result of the `unique` query. This is similar to the
; implementation of the `not` special form.
;
; Test your implementation by forming a query that lists all people who
; supervise precisely one person.

(unique (job ?x (computer wizard)))

(unique (job (Bitdiddle Ben) (computer wizard)))

(unique ?x)

(and (job ?x ?j)
     (unique (job ?anyone ?j)))

(unique (job ?x (computer programmer)))

(and (supervisor ?x ?y)
     (unique (supervisor ?anyone ?y)))

(load "ch4-query.scm")

; This should take as input the `contents` (`cdr`) of the `unique` query,
; together with a stream of frames.
(define (uniquely-asserted query frame-stream)
  ; For each frame in the stream, it should use `qeval` to find the stream of
  ; all extensions to the frame that satisfy the given query.
  ;
  ; Any stream that does not have exactly one item in it should be eliminated.
  ; The remaining streams should be passed back to be accumulated into one big
  ; stream that is the result of the `unique` query. This is similar to the
  ; implementation of the `not` special form.
  (stream-flatmap ; (lambda (frame) frame)
                  (lambda (frame)
                    (let ((qevaled-frame (qeval (car query)
                                                (singleton-stream frame))))
                      (if
                        ; (not (stream-null? frame))
                        (and (not (stream-null? qevaled-frame))
                             (stream-null? (stream-cdr qevaled-frame)))
                        (singleton-stream frame)
                        the-empty-stream)))
                  frame-stream))

(put 'unique 'qeval uniquely-asserted)

(initialize-data-base microshaft-data-base)

(query-driver-loop)

