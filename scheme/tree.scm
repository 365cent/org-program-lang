#lang scheme
'(#\R (#\X ()()) (#\Y (#\Z ()()) (#\W ()())))
(define tree '(#\R (#\X () ()) (#\Y (#\Z () ()) (#\W () ()))))


(define (print-tree t)
  (if (null? t)
      (display "()")
      (begin
        (display "(")
        (if (pair? (car t))
            (print-tree (car t))
            (display (car t)))
        (display " ")
        (print-tree (cdr t))
        (display ")"))))

(print-tree tree)
