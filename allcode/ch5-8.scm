; The following register-machine is ambiguous, because the label `here` is
; defined more than once:
;
; ...
;
; With the simulator as written, what will the contents of register `a` be when
; control reaches `there`? Modify the `extract-labels` procedure so that the
; assembler will signal an error if the same label name is used to indicate two
; different locations.
;
; ---
;
; The contents of register `a` when control reach `there` is `3`.

(load "ch5-regsim.scm")

(define (extract-labels text receive)
  ; Takes as arguments a list `text` (the sequence of controller instruction
  ; expressions) and a `receive` procedure.
  ;
  ; `receive` will be called with two values:
  ;
  ; 1. a list `insts` of instruction data structures, each containing an
  ; instruction from `text`
  ; 2. a table called `labels`, which associates each label from `text` with the
  ; position in the list `insts` that the label designates.
  ;
  ; `extract-labels` works by sequentially scanning the elements of the `text`
  ; and accumulating the `insts` and the `labels`. If an element is a symbol
  ; (and thus a label) an appropriate entry is added to the `labels` table.
  ; Otherwise the element is accumulated onto the `insts` list.
  (if (null? text)
    (receive '() '())
    (extract-labels
      (cdr text)
      (lambda (insts labels)
        (let ((next-inst (car text)))
          (if (symbol? next-inst)
            (if (label-existed? next-inst labels)
              (error "Label existed -- EXTRACT-LABELS" next-inst)
              (receive insts
                       (cons (make-label-entry next-inst
                                               insts)
                             labels)))
            (receive (cons (make-instruction next-inst)
                           insts)
                     labels)))))))

(define (label-existed? label-name labels)
  (assoc label-name labels))

(define ambiguous-machine
  (make-machine
    '(a)
    (list
      )
    '(
      start
      (goto (label here))
      here
      (assign a (const 3))
      (goto (label there))
      ; here
      (assign a (const 4))
      (goto (label there))
      there
      )
    ))

(start ambiguous-machine)

(get-register-contents ambiguous-machine 'a)

(assoc 1 '((1 2) (2 3) (4 5)))

(assoc 'key '((key 2) (2 3) (4 5)))
