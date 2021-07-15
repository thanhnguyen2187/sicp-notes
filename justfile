create-exercise CHAPTER NUMBER:
    cp ./template.rkt chapter-{{CHAPTER}}/exercise-{{CHAPTER}}.{{NUMBER}}.rkt

create-note CHAPTER NUMBER:
    touch chapter-{{CHAPTER}}/note-{{CHAPTER}}.{{NUMBER}}.md

create-scratch NAME:
    cp ./template.rkt scratches/{{NAME}}.rkt

run-exercise CHAPTER NUMBER:
    racket chapter-{{CHAPTER}}/exercise-{{CHAPTER}}.{{NUMBER}}.rkt

run-scratch NAME:
    racket scratches/{{NAME}}.rkt

create-br NAME:
    cp ./template-br.rkt beautiful-racket/{{NAME}}.rkt

run-br NAME:
    racket ./beautiful-racket/{{NAME}}.rkt
