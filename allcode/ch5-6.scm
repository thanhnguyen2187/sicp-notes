; Ben Bitdiddle observes that the Fibonacci machine's controller sequence has an
; extra `save` and an extra `restore`, which can be removed to make a faster
; machine. Where are these instructions?
;
; ---
;
; The instruction is `(restore continue)` and `(save continue)` in
; `afterfib-n-1`.

