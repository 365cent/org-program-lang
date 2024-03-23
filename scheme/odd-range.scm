#lang scheme
(cons 'a '(b))
(car '(a b))
(car '(a b c))
(cdr '(a))
(cdr (append '(a b) '(c d 'a)))

(define (range start end)
  (if (> start end)
      '()
      (cons start (range (+ start 1) end))))

(define (squared-odd-range start end)
  (map (lambda (i) (* i i))
       (filter (lambda (i) (= (modulo i 2) 1))
               (range start end))))

(squared-odd-range 1 10)