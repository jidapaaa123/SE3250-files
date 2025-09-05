#lang racket

; (print 6)

; (define (square x) (* x x))
; (print (square 6))



; (println (mymap square '(1 2 3 4 5 6)))


; (displayln (reduce + '(1 2 3 4 5 6)))


(define (make-null)
    (define (call name)
                (if (eq? name 'null?)
                    #f
                    null)
    )
)

; cons. Produces a procedure: Works with our own car, cdr, null?, map
(define (cons first rest)
    (define (call name)
        (if (eq? name 'car)
            first
            (if (eq? name 'cdr)
                rest
                (if (eq? name 'null?)
                    #f
                    null
                )
            )
        )
    )
    call
)

(define (car lst) (lst 'car))
(define (cdr lst) (lst 'cdr))
(define (null? lst) (lst 'null?))
(define (print lst) (lst 'print))

; map. It will still print out the cons-list as #<procedure:call, though>
(define (map f lst) 
    (if (null? lst)
        null
        (cons 
            ; append/cons mapped first item with mapped rest of list
            (f (car lst)) 
            (map f (cdr lst))
        )
    )
)

; reduce. Doesn't work with our self-implemented cons... only with built-in lists
(define (reduce f values)
    (if (null? (cdr values))
        ; if last item, return it
        (car values)
        ; ELSE -> reduce the first 2 items,
        ;         append the reduced result to rest of list,
        ;         then apply reduce on this new list
        (reduce f (cons 
                    (f (car values) (cadr values))
                    (cddr values)
                  )
        )
    )
)

; filter
(define (filter pred lst)
    (if (pred (car lst)) 
        ; if first item is true
        (
            ; construct new list with first item and recursive result on rest
            cons (car lst) (filter pred (cdr lst))
        )
        ; ELSE -> construct new list with recursive result on rest
        (filter pred (cdr lst))
    )
)