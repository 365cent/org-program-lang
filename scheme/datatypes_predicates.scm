#lang scheme

; A Scheme form is somthing that you as Scheme to evaluate

1
"Hello World!"
(+ 1 2 )
(+ 1 2 3 4)
(- 1 (- 3 2) (+ -4 5))

; Comments start with a semicolon
#; (Comment out an entire form)
#; (define i-am-ignored
     (lambda(x)
       (* x 0)))

(define mynumber 10)
(define myname "Wade")
(string? myname)
(define 1#$&%FV 5)
(+ 1#$&%FV 10)

(newline)
"Data Types"
1           ; integer
3.4         ; real numbers
"Hello, World"
*
#t
#f
#\A

(newline)
(symbol? "Hello")
(symbol? 'World)
(string? "Hello")
(string? 'World)
(symbol? 5)
(symbol? '5)

(newline)
