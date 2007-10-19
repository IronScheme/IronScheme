


(define let (macro (args . body) 
  `((lambda ,(map first args) ,@body) ,@(map second args))))

(define define-macro (macro (nargs . body)
  (let ((name (car nargs))
        (args (cdr nargs)))
  `(define ,name (macro ,args ,@body)))))

(define-macro (foo a . b)
  `'(,a ,@b . 99))

(display
(foo 1 2 3 4)
)











(newline)
(display "Press any key to exit.")
(read-char)
