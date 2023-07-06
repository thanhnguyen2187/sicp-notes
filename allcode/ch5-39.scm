; Write a procedure `lexical-address-lookup` that implements the new lookup
; operation. It should take two arguments--a lexical address and a run-time
; environment--and return the value of the variable stored at the specified
; lexical address. `lexical-address-lookup` should signal an error if the value
; of the variable is the symbol `*unassigned*`. Also write a procedure
; `lexical-address-set!` that implements the operation that changes the value of
; the variable at a specified lexical address.

(define (make-lexical-address frame-number displacement-number)
  (list frame-number displacement-number))

(define (lexical-address-lookup
          run-time-env
          address
          ; the run-time environment is a list of frames, each
          ; containing a list of variables
          )
  (if (null? run-time-env)
    '*unassigned*
    (let ((frame-number (car address))
          (displacement-number (cadr address))
          (frame (car run-time-env)))
      (cond ((null? frame) '*unassigned*)
            ((and (= frame-number 0) (= displacement-number 0))
             (car frame))
            ((and (= frame-number 0) (> displacement-number 0))
             (lexical-address-lookup
               (make-lexical-address frame-number (- displacement-number 1))
               (list (cdr frame))))
            ((> frame-number 0)
             (lexical-address-lookup
               (make-lexical-address (- frame-number 1) displacement-number)
               (cdr run-time-env))))))
  )

(lexical-address-lookup
  '(0 0)
  '((1 2 3)
    (4 5 6)))

(define (lexical-address-set!
          run-time-env
          address
          value)
  (if (null? run-time-env)
    (error "LEXICAL-ADDRESS-SET! received null run-time-env")
    (let ((frame-number (car address))
          (displacement-number (cadr address))
          (frame (car run-time-env)))
      (cond ((null? frame) (error "LEXICAL-ADDRESS-SET! received null frame"))
            ((and (= frame-number 0) (= displacement-number 0))
             (begin
               (set-car! frame value)
               'ok))
            ((and (= frame-number 0) (> displacement-number 0))
             (lexical-address-set!
               (list (cdr frame))
               (make-lexical-address frame-number (- displacement-number 1))
               value))
            ((> frame-number 0)
             (lexical-address-set!
               (cdr run-time-env)
               (make-lexical-address (- frame-number 1) displacement-number)
               value))))))

(define sample-env '((1 2 3) (4 5 6)))

(lexical-address-set! sample-env '(0 0) 0)

sample-env

