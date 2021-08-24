; Exercise 2.71
;
; Suppose we have a Huffman tree for an alphabet of /n/ symbols, and that the
; relative frequencies of the symbols are 1, 2, 4, ..., 2^(n - 1). Sketch the
; tree for /n = 5/; for /n = 10/. In such a tree (for general /n/), how many
; bits are required to encode the most frequent symbol? The least frequent
; symbol?
;
; ---
;
; 1 bit for the most frequent symbol, and 4 for the least frequent.

#lang sicp

; {A B C D E} 31
; -> {A B C D} 15
; ---> {A B C} 7
; -----> {A B} 3
; -------> {A} 1
; -------> {B} 2
; -----> {C} 4
; ---> {D} 8
; -> {E} 16

; {A B C D E F G H I J} 1023
; -> {A B C D E F G H I} 511
; ---> {A B C D E F G H} 255
; -----> {A B C D E F G} 127
; -------> {A B C D E F} 63
; ---------> {A B C D E} 31
; -----------> {A B C D} 15
; -------------> {A B C} 7
; ---------------> {A B} 3
; -----------------> {A} 1
; -----------------> {B} 2
; ---------------> {C} 4
; -------------> {D} 8
; -----------> {E} 16
; ---------> {F} 32
; -------> {G} 64
; -----> {H} 128
; ---> {I} 256
; -> {J 512}
