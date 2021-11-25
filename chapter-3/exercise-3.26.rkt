; Exercise 3.26
;
; To search a table as implemented above, one need to scan through the list of
; records. This is basically the unordered list representation of Section 2.3.3.
; For large tables, it may be more efficient to structure the table in a
; different manner.
;
; Describe a table implementation where the (key, value) records are organized
; using a binary tree, assuming that keys can be ordered in some way (e.g.,
; numerically or alphabetically).
;
; (Compare Exercise 2.66 of Chapter 2.)
;
; ---
;
; For that implementation, we need to replace `assoc`, and `insert-single!`, and
; let the underlying data structure a binary tree instead of a list.

#lang sicp


