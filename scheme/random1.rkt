#lang scheme
#;(let ((x 1))
  (let ((x (+ x 1)))
    (+ x x)))

#| 123 |#

;#

(let ((x 1) (y 10))
  (let ((x (+ y (* x 1))))
    (+ x (- (let ((x (+ x y)) (y (* y y)))
              (- y x)) y ))))

(define sum
        (lambda (ls)
          (if (null? ls)
              0
              (+ (car ls) (sum (cdr ls))))))

#;(let ((sum (lambda (ls)
                (if (null? ls)
                    0
                    (+ (car ls) (sum (cdr ls)))))))
(sum '(1 2 3 4 5)))

(define list '(1 2 3 4 5))

((lambda x (sum x)) 1 2 3 4)

;(sum '(1 2 3 4 5 6))



