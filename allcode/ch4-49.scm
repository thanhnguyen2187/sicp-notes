; Alyssa P. Hacker is more interested in generating sentences than in parsing
; them. She reasons that by simply changing the procedure `parse-word` so that
; it ignores the "input sentence" and instead always succeeds and generate an
; appropriate word, we can use the programs we had built for parsing to do
; generation instead. Implement Alyssa's idea, and show the first half-dozen or
; so sentences generated.

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items)
       (an-element-of (cdr items))))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  ; (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (an-element-of (cdr word-list))))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(parse '(1 2 3))
; (sentence (simple-noun-phrase (article the) (noun student)) (verb studies))
; (sentence (simple-noun-phrase (article the) (noun student)) (verb lectures))
; (sentence (simple-noun-phrase (article the) (noun student)) (verb eats))
; (sentence (simple-noun-phrase (article the) (noun student)) (verb sleeps))
; ...

(parse '(1 2 3 4 5 6))
; (sentence
;   (simple-noun-phrase
;     (article the)
;     (noun student))
;   (verb-phrase
;     (verb studies)
;     (prep-phrase
;       (prep for)
;       (simple-noun-phrase
;         (article the)
;         (noun student)))))
; (sentence
;   (simple-noun-phrase
;     (article the)
;     (noun student))
;   (verb-phrase
;     (verb studies)
;     (prep-phrase
;       (prep for)
;       (simple-noun-phrase
;         (article the)
;         (noun professor)))))
; ...

try-again
