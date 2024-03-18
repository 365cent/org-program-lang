#lang scheme

; Comments begin with a semi-colon
; Clicking "Run in Dr Racket will evaluate your program

; Lieterals
"Hello, World" ; String
10             ; Integer
3.14           ; Real
'HelloWorld    ; Symbol

; Scheme uses prefix notation: the operator coems first
(+ 1 2)
(sqrt (+ (* 4 4) (* 3 3 )))  ; square root of (3 * 3) + (4 * 4)
(define x 5)
(* 2 x)

; Define a new function
(define half-it
  (lambda (x)
    (/ x 2)))
(half-it 72)

; Lists
(list 1 2 3 4)
(define 1st (list 1 2 3 4))
(define 1st2 '(a b c d)) ; The quote tells Scheme not to evaluate what is in parenthesis
1st
1st2
(append (list 1 2 3 4) '(5 6 7 8))
