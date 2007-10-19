


(define let (macro (args . body) 
  (if (symbol? args)
    ((lambda (name args body)
      `((lambda (,name)
        (set! ,name (lambda ,(map first args) ,@body))
        (,name ,@(map second args))) #f))
        args (car body) (cdr body))
    `((lambda ,(map first args) ,@body) ,@(map second args)))))

    
    
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

(let bar ((a 1))
  (if (null? a)
    (display "eureka!")
    (bar '())))









(newline)
(display "Press any key to exit.")
(read-char)
