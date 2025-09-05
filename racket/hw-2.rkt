#lang racket

(define (is-quoted string)
    ; returns true if first item of string is 'quote
    (eq? (car string) (quote quote))
)

(define (is-if string)
    ; returns true if first item of string is 'if
    (eq? 
        ; (begin
            (car string)
            ; (display 'is )
            ; (displayln (car string))
        ; )
         (quote if))
)

; supports comparison between 2 items: '='
(define (is-true string)
    ; ex:  (is-true '(= 2 2)) => #t
    ; condition = '(= 2 2) => string
    ; operation = '=' => (car string)
    ; operand1 = '2' => (cadr string)
    ; operand2 = '2' => (caddr string)
    (define operation (car string))
    (define operand1 (cadr string))
    (define operand2 (caddr string))

    (cond
        ( (eq? operation '=)
            (= operand1 operand2)
        )
    )




)

; evaluates the quoted/string-ed "code"
(define (eval string)
    (cond
        ( (is-quoted string) 
            (cadr string)
        )
        ( (is-if string)
            ; determine if condition is true or false
                ; ex: '(if (= 2 2) 'yes 'no)
                ; condition clause = (= 2 2) => (cadr string)
                ; result_true = 'yes => (caddr string)
                ; result_false = 'no => (cadddr string)
                (define condition (cadr string))
                (define result_true (caddr string))
                (define result_false (cadddr string))
                (displayln '(I made it past definitions))
                ; (displayln '(condition))
                (cond
                    ( (eq? #t condition)
                        (begin
                            (displayln '(I made it past evaluation: true))
                            (displayln(result_true))
                        )
                    )
                    (  else
                        (begin
                            (displayln '(I made it past evaluation: false))
                            (displayln(result_false))
                        )
                       
                    )
                
                )
        )
    )
)

(if (= 2 2) 'yes 'no)
(cadr '(if (= 2 2) 'yes 'no))
(caddr '(if (= 2 2) 'yes 'no))
(cadddr '(if (= 2 2) 'yes 'no))
(eval '(if (= 2 2) 'yes 'no))