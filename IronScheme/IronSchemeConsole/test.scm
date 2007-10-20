
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
  
(define-macro (begin . e)
  `((lambda () ,@e)))

(let bar ((a '(1 2 3)))
  (if (null? a)
    (begin (display "eureka!")(newline))
    (bar (cdr a))))
    

      









(newline)
(display "Press any key to exit.")
(read-char)
