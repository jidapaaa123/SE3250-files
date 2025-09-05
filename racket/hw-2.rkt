#lang racket

(define (is-quoted string)
    ; returns true if first item of string is 'quote
    (eq? (car string) (quote quote))
)

(define (is-if string)
    ; returns true if first item of string is 'if
    (eq? 
        (car string)
        (quote if))
    
)

(define (is-lambda string)
    ; ex: '(lambda () (displayln 'Helloooo)) => #t
    (eq?
        (car string)
        (quote lambda)
    )
)

(define (is-displayln string)
    ; ex: '(displayln 'hi) => 'hi
    (eq?
        (car string)
        (quote displayln)
    )
)


; supports comparison between 2 items: =, <, >, <=, >=, eq?
(define (is-true? string)
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
        ( (eq? operation '<)
            (< operand1 operand2)
        )
        ( (eq? operation '>)
            (> operand1 operand2)
        )
        ( (eq? operation '<=)
            (<= operand1 operand2)
        )
        ( (eq? operation '>=)
            (>= operand1 operand2)
        )
        ( (eq? operation 'eq?)
            (eq? operand1 operand2)
        )
    )
)


; niche issue: turn ''yes => 'yes
(define (unwrap text)
    (cadr text)
)

(define (fill-parameter body param value)
    (map 
        (lambda (x)
            (if (eq? x param)
             value
             x
            )
        )
       body
    )
)

(define (is-fill-parameter string)
    (eq?
        (car string)
        (quote fill-parameter)
    )
)


; evaluates the quoted/string-ed "code"
; current capabilities: quote, if, 
(define (eval string)
    (cond
        ; BASIC CASES: displayln, display
        ( (is-displayln string)
            (cadr string)
            ; (unwrap (cadr string))
        )
        ( (is-quoted string) 
            (cadr string)
        )
        ( (is-fill-parameter string)
            ; ex: '(fill-parameter (displayln str) str okkkk)
            (define body (cadr string))
            (define param_name (caddr string))
            (define param_value (cadddr string))
            (define result (fill-parameter body param_name param_value))
            (result)
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
                (cond
                    ( (is-true? condition)
                        (unwrap result_true)
                    )
                    (  else
                        (unwrap result_false)
                    )
                )
        )
        ( (is-lambda string)
            ; ex: '(lambda () (displayln 'Helloooo))
            ; parameters = '() = (cadr string)
            ; body = '(displayln 'Helloooo) = (caddr string)
            (define parameters (cadr string))
            (define body (caddr string))

            (cond
                ; case 1: 0 parameter
                ; ex:
                    ; (define greet (lambda () (displayln 'Helloooo)))
                    ; (greet) => "Helloooo"
                ( (empty? parameters)
                    (define (call) (eval body))
                    call
                )

                ; case 2: 1 parameter
                ; ex:
                    ; (define echo (lambda (str) (displayln str)))
                    ; param_name = str => (car parameters)
                ( else
                    (define (call param) 
                        (eval `(fill-parameter ,body ,(car parameters) ,param))
                    )

                    call
                )
            ) 
        )
    )
)


; (if (= 2 2) 'yes 'no)
; (eval '(if (< 2 2) 'yes 'no))
; (displayln '(hi hello how r u))
; (eval '(displayln '(hi hello how r u)))
; (define greet (eval '(lambda () (displayln 'Helloooo))))
; (greet)

; (define bye (eval '(lambda () (displayln 'Byeee))))
; (bye)
; bye

; (define err (eval '(lambda () (display 'Byeee))))
; (err 'hi)

(define echo (eval '(lambda (str) (displayln str))))
(echo 'okkkk)

