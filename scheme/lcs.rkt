#lang scheme

(define lcs
  (lambda args
    (if (< (length args) 2)
      (if (null? args) '() (car args))
      (let ((lst1 (car args))
           (lst2 (cadr args)))
        (define (ll lst)
          (if (null? lst)
            0
            (+ 1 (ll (cdr lst)))
          )
        )
        (define (lcsp lst1 lst2)
          (if (or (null? lst1) (null? lst2))
            '()
            (let ((head1 (car lst1))
                  (tail1 (cdr lst1))
                  (head2 (car lst2))
                  (tail2 (cdr lst2)))
              (if (equal? head1 head2)
                (cons head1 (lcsp tail1 tail2))
                (let ((option1 (lcsp tail1 lst2))
                    (option2 (lcsp lst1 tail2)))
                  (if (> (length option1) (length option2))
                    option1
                    option2
                  )
                )
              )
            )
          )
        )
        (lcsp lst1 lst2)
      )
    )
  )
)

;; Example usage:

(lcs '() '(a b))
(lcs '(b a) '(a b))
(lcs '(a b c b d a b) '(b d c a b a))
