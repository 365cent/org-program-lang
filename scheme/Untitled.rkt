#lang scheme
  
#;(define lcs
  (lambda args
    (let ((lst1 (car args))
         (lst2 (cadr args)))
      (if (or (null? lst1) (null? lst2))
          '()
          (let ((head1 (car lst1))
            (tail1 (cdr lst1))
            (head2 (car lst2))
            (tail2 (cdr lst2)))
          (if equal? head1 head2)
            (cons head1 (lcs tail1 tail2))
            (let ((option1 (lcs tail1 lst2))
                  (option2 (lcs lst1 tail2)))
              (if (> (length option1) (length option2))
                  option1
                  option2
              )
           )
            
          )
      )
    )
  )
)

(define (lcs list1 list2)
  ;; Base case: one of the lists is empty.
  (if (or (null? list1) (null? list2))
      '()
      (let ((head1 (car list1))
            (tail1 (cdr list1))
            (head2 (car list2))
            (tail2 (cdr list2)))
        (if (equal? head1 head2)
            ;; If the heads are equal, include this element and continue with the tails.
            (cons head1 (lcs tail1 tail2))
            ;; Else, find the longest subsequence by either skipping an element from list1 or list2.
            (let ((option1 (lcs tail1 list2))
                  (option2 (lcs list1 tail2)))
              (if (> (length option1) (length option2))
                  option1
                  option2))))))

(lcs '(a b c d b a b) '(b d c a b a))

