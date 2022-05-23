; With the grammar given above, the following sentence can be parsed in five
; different ways: "The professor lectures to the student in the class with the
; class." Give the five parses and explain the difference in shades of meaning
; among them.
;
; ---
;
; The shades of meaning differentiate each other in how is the student defined,
; and also on how "with the cat" is used.

(parse '(the professor lectures to the student in the class with the cat))

try-again

(sentence
  (simple-noun-phrase
    (article the)
    (noun professor))
  (verb-phrase
    (verb-phrase
      (verb-phrase
        (verb lectures)
        (prep-phrase
          (prep to)
          (simple-noun-phrase
            (article the)
            (noun student))))
      (prep-phrase
        (prep in)
        (simple-noun-phrase
          (article the)
          (noun class))))
    (prep-phrase
      (prep with)
      (simple-noun-phrase
        (article the)
        (noun cat)))))

(sentence
  (simple-noun-phrase
    (article the)
    (noun professor))
  (verb-phrase
    (verb-phrase
      (verb lectures)
      (prep-phrase
        (prep to)
        (simple-noun-phrase
          (article the)
          (noun student))))
    (prep-phrase
      (prep in)
      (noun-phrase
        (simple-noun-phrase
          (article the)
          (noun class))
        (prep-phrase
          (prep with)
          (simple-noun-phrase
            (article the)
            (noun cat)))))))

(sentence
  (simple-noun-phrase
    (article the)
    (noun professor))
  (verb-phrase
    (verb-phrase
      (verb lectures)
      (prep-phrase
        (prep to)
        (noun-phrase
          (simple-noun-phrase
            (article the)
            (noun student))
          (prep-phrase
            (prep in)
            (simple-noun-phrase
              (article the)
              (noun class))))))
    (prep-phrase
      (prep with)
      (simple-noun-phrase
        (article the)
        (noun cat)))))

(sentence
  (simple-noun-phrase
    (article the)
    (noun professor))
  (verb-phrase
    (verb lectures)
    (prep-phrase
      (prep to)
      (noun-phrase
        (noun-phrase
          (simple-noun-phrase
            (article the)
            (noun student))
          (prep-phrase
            (prep in)
            (simple-noun-phrase
              (article the)
              (noun class))))
        (prep-phrase
          (prep with)
          (simple-noun-phrase
            (article the)
            (noun cat)))))))

(sentence
  (simple-noun-phrase
    (article the)
    (noun professor))
  (verb-phrase
    (verb lectures)
    (prep-phrase
      (prep to)
      (noun-phrase
        (simple-noun-phrase
          (article the)
          (noun student))
        (prep-phrase
          (prep in)
          (noun-phrase
            (simple-noun-phrase
              (article the)
              (noun class))
            (prep-phrase
              (prep with)
              (simple-noun-phrase
                (article the)
                (noun cat)))))))))
