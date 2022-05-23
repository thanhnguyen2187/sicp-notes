; Extend the grammar given above to handle more complex sentences. For example,
; you could extend noun phrases and verb phrases to include adjectives and
; adverbs, or you could handle compound sentences.

(define adjectives '(adjective quick old black))

(parse '(the cat eats))

(parse '(the black cat sleeps))

(define (parse-simple-noun-phrase)
  (amb (list 'simple-noun-phrase
             (parse-word articles)
             (parse-word adjectives)
             (parse-word nouns))
       (list 'simple-noun-phrase
             (parse-word articles)
             (parse-word nouns))))

try-again

