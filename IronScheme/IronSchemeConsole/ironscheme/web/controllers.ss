(library (ironscheme web controllers)
  (export
    get
    post
    define-action)
  (import
    (ironscheme)
    (ironscheme web))
    
  (define-syntax define-aux
    (syntax-rules ()
      [(_ id ...)
        (begin
          (define-syntax id
            (lambda (x)
              (syntax-violation #f "invalid use of auxiliary keyword" x 'id))) ...)]))
            
  (define-aux get post)                

  (define (get-value key)
    (or (context-item key) (form key) (querystring key)))
    
  (define-syntax define-action
    (lambda (x)
      (syntax-case x ()
        [(_ (name arg ...) body body* ...)
          #'(define (name)
              ((lambda (arg ...)
                body body* ...) 
                (get-value 'arg) ...))]
        [(_ name [(action arg ...) body body* ... ] ...)
          (for-all 
            (lambda (i)
              (or
                (free-identifier=? i #'get)
                (free-identifier=? i #'post)))
            #'(action ...))
          #'(define (name)
              (case (http-method)
                [(action) 
                  ((lambda (arg ...)
                      body body* ...) 
                    (get-value 'arg) ...)] ...))])))     
)    