;

#lang br/quicklang

; reader: convert the source code of our language
;         from a string of characters into Racket-style parenthesized forms, or 
;         S-expressions.
; expander: determine how the parenthesized forms correspond to real Racket
;           expressions (and which are then evaluated to produce a result)
;
; every reader must export a read-syntax function
; Racket passes two arguments to `read-syntax`:
; - the `path` to the source file
; - a port for reading data from the file

(define (read-syntax path port) ; set up a function
  (define src-lines (port->lines port)) ; read everything from the port at once
  ;;; read lines
  (define src-datums (format-datums '(handle ~a) src-lines))
  ;;; turn each of line into (handle value)
  ; takes a list of
  ; strings and converts each using a `format string`.
  ; - `~a` marks the place where the argument string will be substituted
  ; - "4" becomse '(handle 4)
  ; - "+" becomse '(handle +)
  (define module-datum `(module stacker-mod "stacker.rkt" ,@src-datums))
  ;;; define module as the transformed lines
  ; the backtick ` is called quasiquote
  ; - it works mostly the same way as the usual quote prefix '
  ; - the quasi part is that we can insert variables into the list
  ; - to insert a single value, we use the unquote operator, which is a comma ,
  ; followed by the variable
  ; - to insert a list of multiple values, we use the unquote-splicing operator,
  ; which is a comma and at sign ,@ followed by the variable.
  (datum->syntax #f module-datum))

; module name is arbitrary
; the expander determines how the expresisons inside the module are interpreted
; convert the module code into a `syntax object` (a way of treating code as data):
; 1. turn the expression into a datum, which is the raw representation of the code
;    as it appears in the source
; 2. use ' (or quote) to make a datum
; 3. update datum to syntax object by `datum->syntax`
;    the first argument is the program context
;    the second one is the datum itself

(provide read-syntax) ; export the function
; 
(define-macro (stacker-module-begin HANDLE-EXPR ...)
              #'(#%module-begin
                 HANDLE-EXPR ...
                 (display (first stack))))
              ; #' make code into a syntax object
(provide (rename-out [stacker-module-begin #%module-begin]))

(define stack empty)

(define (pop-stack!)
  (define arg (first stack))
  (set! stack (rest stack))
  arg)

(define (push-stack! arg)
  (set! stack (cons arg stack)))

(define (handle [arg #f])
  (cond
    [(number? arg) (push-stack! arg)]
    [(or (equal? + arg) (equal? * arg))
     (define op-result (arg (pop-stack!) (pop-stack!)))
     (push-stack! op-result)]))
(provide handle)
(provide + *)
