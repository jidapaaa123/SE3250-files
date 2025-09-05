#lang racket

(define (lookup-by-name name dict)
    (if (null? dict)
        null
        (if (eq? name (car (car dict))) ; check first entry's key 
            (cadr (car dict)) ; return first entry's value
            (lookup-by-name name (cdr dict)) ; look through the rest of the dict's key-val pairs
        )
    )
)

(define (eval code)
    (define thing (lookup-by-name code primitives))
    (define code (if (null? thing) code thing))
    

    (define primitives
        (list
            (list (quote +) +)
            (list (quote -) -)
        )
    )
    (define operator (lookup-by-name (car code) primitives))
    (operator
        (cadr code)
        (caddr code))
)


(displayln (eval (quote (+ 3 2))))