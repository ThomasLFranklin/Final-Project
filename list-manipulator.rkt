;  This is a temporary Racket file to be stored here until it is merged and implemented into our program. Once this is done,
;    delete this file.
(define key-list (list "plholder" #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))

(define (list-change list idx value)
  (if (null? list)
    list
    (cons
      (if (zero? idx)
        value
        (car list))
      (list-change (cdr list) (- idx 1) value))))

(list-change key-list 2 #t)
