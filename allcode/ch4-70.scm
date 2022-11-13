; What is the purpose of the `let` bindings in the procedures `add-assertion!`
; and `add-rule!`? What would be wrong with the following implementation of
; `add-assertion!`? Hint: Recall the definition of the infinite stream of ones
; in Section 3.5.2: `(define ones (cons-stream 1 ones))`.
;
; ...
;
; Since `cons-stream` is a special form (it is not evaluated until `cdr`, or
; `stream-cdr` is used), `THE-ASSERTIONS` within `(set! THE-ASSERTIONS
; (cons-stream assertion THE-ASSERTIONS))` is going to act like `ones` of
; `(define ones (cons-stream 1 ones))`, thus form an infinite stream that only
; contains the last `assertion`.

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (add-assertion! assertion)
  (store-assertion-in-index asertion)
  (set! THE-ASSERTIONS (cons-stream assertion THE-ASSERTIONS))
  'ok)

(define ones (cons-stream 1 ones))
