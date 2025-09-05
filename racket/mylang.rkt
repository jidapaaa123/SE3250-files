; 9/3 Lecture
#lang racket


(define (is-quoted code)
    (eq? (car code) (quote quote))
)
; support quote, if, lambda{0,1}, call function {0, 1} args
(define (eval code)
    (cond
        ((is-quoted code) 
            (cadr code)
        )
    )
)

(eval '(quote (hello hi this (is cool) code)))


; ; input: ( 2 * 9 - 5 * 7)
; ; goal output: (- (* 2 9) (* 5 7 ))
; (define (transform infix-code)



; )



; ; closure
; (define (cons a b)
;     (lambda () a)
; )