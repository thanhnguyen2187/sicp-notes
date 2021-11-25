; Exercise 3.66
;
; Examine the stream `(pairs integers integers)`. Can you make any general
; comments about the order in which paris are placed into the stream?
;
; For example, approximately how many pairs precede the pair (1, 100)? the pair
; (99, 100)?, the pair (100, 100)?
;
; (If you can make precise mathematical statements here, all the better. But
; feel free to give more qualitative answers if you find yourself getting bogged
; down)
;
; ---
;
; Before S_0, T_j is (j * (j + 1)) / 2 elements.
; Before S_i, T_j is (j * (j + 1)) / 2 + i elements.

#lang sicp
