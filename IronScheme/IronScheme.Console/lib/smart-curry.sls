(library (smart-curry)
	(export 
		smart-curry smart-aux) ;; must export samrt-aux for eval to work
	(import
		(ironscheme)) ;; for procedure-arity

  (define-syntax curry
    (lambda (x)
      (syntax-case x ()
        [(_ proc arg ...)
          (let f ((args #'(arg ...)))
            (if (null? args)
                #'(lambda () (proc arg ...))
                #`(lambda (#,(car args)) #,(f (cdr args)))))])))

  (define-syntax smart-aux
    (lambda (x)
      (syntax-case x ()
        [(_ n)
          (with-syntax (((arg ...) (generate-temporaries 
                                     (make-list (syntax->datum #'n)))))
            #'(lambda (f) (curry f arg ...)))])))

  (define (smart-curry x)
    ;; todo all kinds of checks
    (let ((n (procedure-arity x)))
      ((eval `(smart-aux ,n) (environment '(smart-curry))) x))))