; Exercise 3.25
;
; Generalizing one- and two-dimensional tables, show how to implement a table in
; which values are stored under an arbitrary number of keys and different values
; may be stored under different numbers of keys. The `lookup` and `insert!`
; procedures should take as input a list of keys used to access the table.

#lang sicp

(define (type value)
  (cond ((number? value) 'number)
        ((symbol? value) 'symbol)
        (value 'type)))

(define (make-kv-pair key value)

  (define (set-value! new-value)
    (set! value new-value))

  (define (dispatch message)
    (cond ((eq? message 'key)   key)
          ((eq? message 'value) value)
          ((eq? message 'set-value!) set-value!)
          ((eq? message 'type) 'kv-pair)
          (else (error "Unknown operation: MAKE-KV-PAIR" message))))

  dispatch)

(define (make-ptr value)

  (define (dispatch message)
    (cond ((eq? message 'value) value)
          ((eq? message 'set-value!) (lambda (new-value)
                                       (set! value new-value)))
          ((eq? message 'type) 'ptr)
          (else (error "Unknown operation: MAKE-PTR" message))))

  dispatch)

(define (make-table same-key?)

  (let ((kv-pairs-ptr (make-ptr (list)))
        ; A pointer is needed for `insert!` and `insert!`
        )

    (define (assoc key kv-pairs)
      (if (null? kv-pairs)
          false
          (let ((kv-pair (car kv-pairs)))
            (if (same-key? key (kv-pair 'key))
                kv-pair
                (assoc key (cdr kv-pairs))))))

    (define (lookup-single key kv-pairs)
      (let ((kv-pair (assoc key kv-pairs)))
        (if kv-pair
            (kv-pair 'value)
            false)))

    (define (lookup keys kv-pairs)
      ; Assume that ther is at least one key. If there is more keys, call the
      ; function again with those keys and the result of lookup-ing one key,
      ; which is potentially key-value pairs.
      (let ((key (car keys))
            (rest-keys (cdr keys)))
        (let ((lookup-result (lookup-single key kv-pairs)))
          (if (and (not (null? rest-keys))
                   (eq? (type lookup-result) 'ptr))
              (let ((rest-kv-pairs lookup-result))
                (lookup rest-keys rest-kv-pairs))
              lookup-result))))

    (define (insert-single! key value kv-pairs-ptr)
      (let ((kv-pairs (kv-pairs-ptr 'value)))
        (let ((kv-pair (assoc key kv-pairs)))
          (if kv-pair
              ((kv-pair 'set-value!) value)
              ((kv-pairs-ptr 'set-value!) (cons (make-kv-pair key value)
                                                kv-pairs)))
        'ok)))

    (define (insert! keys value kv-pairs-ptr)
      ; Assume that there is at least one key.
      (let ((kv-pairs (kv-pairs-ptr 'value)))
        (let ((key (car keys))
              (rest-keys (cdr keys)))
          (if (null? rest-keys)
              (insert-single! key value kv-pairs-ptr)
              (let ((rest-kv-pairs (cdr kv-pairs)))
                (let ((rest-kv-pairs-ptr (make-ptr rest-kv-pairs)))
                  (insert-single! key rest-kv-pairs-ptr kv-pairs-ptr)
                  (insert! rest-keys value rest-kv-pairs-ptr))))))
      'ok)

    (define (dispatch message)
      (cond ((eq? message 'lookup) (lambda (keys)
                                     (lookup keys (kv-pairs-ptr 'value))))
            ((eq? message 'lookup-single) (lambda (key)
                                            (lookup-single key (kv-pairs-ptr 'value))))
            ((eq? message 'insert!) (lambda (keys value)
                                      (insert! keys value kv-pairs-ptr)))
            ((eq? message 'insert-single!) (lambda (key value)
                                             (insert-single! key value kv-pairs-ptr)))
            ((eq? message 'assoc) (lambda
                                    (key) (assoc key kv-pairs-ptr)))
            ((eq? message 'kv-pairs) kv-pairs-ptr)
            (else (error "Unknown operation: MAKE-TABLE" message))))

    dispatch))

(define table (make-table eq?))

((table 'insert-single!) 2 'two)
((table 'insert-single!) 10 'ten)
((table 'insert!) (list 'number->word 1) 'one)
((table 'insert!) (list 'number->word 2) 'two)
((table 'insert!) (list 'number->word 3) 'three)

(display "=====") (newline)

((table 'lookup-single) 'number->word)
; <procedure:...>
((car (((table 'lookup-single) 'number->word) 'value)) 'value)
; three
; since it was the last inserted value
((table 'lookup) (list 'number->word 3))
; three
((table 'lookup-single) 10)
; ten
((table 'lookup) (list 'number->word 9))
; #f
