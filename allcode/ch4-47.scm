; Louis Reasoner suggests that, since a verb phrase is either a verb or a verb
; phrase followed by a preposition phrase, it would be much more straightforward
; to define the procedure `parse-verb-phrase` as follows (and similarly for noun
; phrases):
;
; ...
;
; Does this work? Does the program's behavior change if we interchange the order
; of expressions in the `amb`?
;
; ---
;
; It does work for the first result, but runs into an infinite loop afterwards,
; since there is another `parse-verb-phrase` within `parse-verb-phrase` which is
; going to make the evaluator runs into an infinite loop.

(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))

(parse '(the cat eats))

(parse '(the professor lectures to the student with the cat))

try-again
