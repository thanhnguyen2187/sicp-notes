; Using `find-variable` from Exercise 5.41, rewrite `compile-variable` and
; `compile-assignment` to output lexical-address instructions.
;
; In cases where `find-variable` returns `not-found` (that is, where the
; variable is not in the compile-time environment), you should have the code
; generators use the evaluator operations, as before, to search for the binding.
; (The only place a variable is not found at compile time can be in the global
; environment, which is part of the run-time environment but is not part of the
; compile-time environment. Thus, if you wish, you may have the evaluator
; operations look directly in the global environment, which can be obtained with
; the operation `(op get-global-environment)`, instead of having them search the
; whole run-time environment and found in `env`.) Test the modified compiler on
; a few simple cases, such as the nested `lambda` combination at the beginning
; of this section.
;
; ---
;
; Look at `ch5-compiler-lexical-address.scm`.

