(library (ironscheme web controllers)
  (export
    define-action)
  (import
    (ironscheme)
    (ironscheme web))

  (define (get-value key)
    (or (context-item key) (form key) (querystring key)))
    
  (define-syntax define-action
    (syntax-rules ()
      [(_ (name arg ...) body body* ...)
        (define (name)
          ((lambda (arg ...)
            body body* ...) 
            (get-value 'arg) ...))]))    
)    