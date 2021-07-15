; In Racket, a "name used in the code" is called an identifier
; "A connection to an actual value or function" is called a binding
; The expander prepares the code for evaluation by ensuring that every
; identifier has a binding.
; Once an identifier has a binding, it becomes a variable.
;
; Within the expander, we have three basic techniques for adding bindings to
; code:
; - define macros that rewrite certain code as other code at compile time
; - define functions that are invoked at runtime
; - import bindings from existing Racket modules, which can include both macros
; and functions

#lang reader "stacker.rkt"

444
222
+
555
+
