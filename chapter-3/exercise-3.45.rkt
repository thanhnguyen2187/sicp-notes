; Exercise 3.45
;
; Louis Reasoner thinsk our bank-account system is unnecessarily complex and
; error-prone now that deposits and withrawals aren't automatically serialized.
; He suggests that `make-account-and-serializer` should have exported the
; serializer (for use by such procedures as serialized-exchange) in addition to
; (rather than instead of) using it to serialize accounts and deposits as
; `make-account` did. He proposes to redefine accounts as follows:
;
; ...
;
; Then deposits are handled with the original `make-account`:
;
; ...
;
; Explain what is wrong with Louis's reasoning. In particular, consider what
; happens when `serialized-exchange` is called.
;
; ---
;
; Since `exchange` includes `withdraw` itself, the call stack will become:
;
; - exchange
; - withdraw
;
; The serializer is the same, so with the `withdraw` invoking, it waits until
; `exchange` finish, while `exchange` is the parent of `withdraw` itself also
; waits for `withdraw`'s finish to finish the lock. They block each other until
; the end of time.

#lang sicp

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

(define (deposit acount amount)
  ((account 'deposit) amount))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange)) account1 account2)))

(define (exchange account1 account2)
  ; This procedure works well when only a single process is trying to do the
  ; exchange. Suppose, however, that Peter and Paul both have access to accounts
  ; `a1`, `a2`, and `a3`, and that Peter exchanges `a1` and `a2` while Paul
  ; concurrently exchanges `a1` and `a3`. Even with account deposits and
  ; withdrawals serialized for individual accounts (as in the `make-account`
  ; procedure shown above in this section), `exchange` can still procedure
  ; incorrect results.
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))



