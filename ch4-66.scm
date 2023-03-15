; Ben has ben generalizing the query system to provide statistics about the
; company. For example, to find the total salaries of all the computer
; programmers one will be able to say
;
; ...
;
; In general, Ben's new system allows expressions of the form
;
; ...
;
; where `accumulation-function` can be things like `sum`, `average`, or
; `maximum`. Ben reasons that it should be a cinch to implement this. He will
; simply feed the query pattern to `qeval`. This will produce a stream of
; frames. He will then pass this stream through a mapping function that extracts
; the value of the designated variable from each frame in the stream and feed
; the resulting stream of values to the accumulation function. Just as Ben
; completes the implementation and is about to try out, Cy walks by, still
; puzzling over the `wheel` query result in Exercise 4.65. When Cy shows Ben the
; system's response, Ben groans, "Oh, no, my simple accumulation scheme won't
; work!"
;
; What has Ben just realized? Outline a method he can use to salvage the
; situation.
;
; ---
;
; Ben has just realized that Oliver Warbucks's salary is going to be added four
; times if he wants to find the total salaries of the company's `wheel`s. A
; `unique` filter can be used to salvage the situation.
