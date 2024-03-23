#lang scheme

(define double-me (lambda (x) (* x 2)))
(double-me 23)

(define (eat-single-apple apple-index)
  ;; Placeholder for actual implementation.
  (display (string-append "Eating apple #" (number->string apple-index) "\n")))

(letrec ((eat-apples (lambda (apple-index num-apples)
                       (if (>= apple-index num-apples)
                           #f
                           (begin
                             (eat-single-apple apple-index)
                             (eat-apples (+ apple-index 1) num-apples))))))
  (eat-apples 0 100))
