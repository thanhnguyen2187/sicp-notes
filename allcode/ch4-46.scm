; The evaluators in Section 4.1 and Section 4.2 do not determine what order
; operands are evaluated in. We will see that the `amb` evaluator evaluates them
; from left to right. Explain why our parsing program wouldn't work if the
; operands were evaluated in some other order.
;
; ---
;
; Our parsing program wouldn't work if the operands were evaluated in some other
; order since `parse-sentence` works from left to right. If the operands were
; evaluated in some other order (e.g. from right to left), `parse-noun-phrase`
; would be called first, and then `parse-verb-phrase`, which is not what we
; want.

