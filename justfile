create CHAPTER NUMBER:
    cp ./template.rkt chapter-{{CHAPTER}}/exercise-{{CHAPTER}}.{{NUMBER}}.rkt

run CHAPTER NUMBER:
    racket chapter-{{CHAPTER}}/exercise-{{CHAPTER}}.{{NUMBER}}.rkt

create-br NAME:
    cp ./template-br.rkt beautiful-racket/{{NAME}}.rkt

run-br NAME:
    racket ./beautiful-racket/{{NAME}}.rkt
