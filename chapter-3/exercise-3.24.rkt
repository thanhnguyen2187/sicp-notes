; Exercise 3.24
;
; In the table implementation above, the keys are tested for equality using
; `equal?` (called by `assoc`). This is not always the appropriate test.
;
; For instance, we might have a table with numeric keys in which we don't need
; and exact match to the number we are looking up, but only a number with some
; tolerance of it.
;
; Design a table constructor `make-table` that takes an
; argument a `same-key?` procedure that will be used to test "equality" of keys.
;
;`make-table` should return a `dispatch` procedure that can be used to access
; appropriate `lookup` and `insert!` procedure for a local table.

#lang sicp

(define (make-record key value)

  (define (get-key)
    key)

  (define (get-value)
    value)

  (define (set-value! key new-value)
    (set! value new-value))

  (define (dispatch message)
    (cond ((eq? message 'key)   (get-key))
          ((eq? message 'value) (get-value))
          ((eq? message 'set-value!) set-value!)
          (else (error "Unknown operation: MAKE-RECORD" message))))

  dispatch)

(define (make-table same-key?)

  (let ((table (list '*table*)))

    (define (assoc key records)
    ; since a record is a pair of key and value,
    ; we use `caar` to get the key within a record from records
    (let ((record (car records)))
      (if (null? records)
          false
          (if (same-key? key (record 'key))
              record
              (assoc key (cdr records))))))

    (define (lookup key)
      (let ((record (assoc key (cdr table))))
        (if record
            (record 'value)
            false)))

    (define (insert! key value)
      (let ((record (assoc key (cdr table))))
        (if record
            ((record 'set-value!) value)
            (set-cdr! table
                      (cons (make-record key value)
                            (cdr table))))))

    (define (dispatch message)
      (cond ((eq? message 'lookup)  lookup)
            ((eq? message 'insert!) insert!)
            (else (error "Unknown operation: MAKE-TABLE" message))))

    dispatch))

