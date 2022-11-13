; Why does `flatten-stream` use `delay` explicitly? What would be wrong with
; defining it as follows:
;
; ...
;
; ---
; 
; `flatten-stream` without delay is going to enumerate through the entire stream
; before returning results, which defeats the purpose of using stream itself.

(define (flatten-stream stream)
  (if (stream-null? stream)
    the-empty-stream
    (interleave
      (stream-car stream)
      (flatten-stream (stream-cdr stream)))))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))


