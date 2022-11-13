; Why do `disjoin` and `stream-flatmap` interleave the streams rather than
; simply append them? Give examples that illustrate why interleaving works
; better. (Hint: Why did we use `interleave` in Section 3.5.3)
;
; ---
;
; `disjoin` and `stream-flatmap` use `interleave` rather than simply
; `stream-append` to make sure that the elements of both streams are used
; in case the first stream is an infinite one.
