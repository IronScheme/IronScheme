(library (ironscheme clr)
  (export
    clr-cast
    clr-call
    clr-new)
  (import 
    (rnrs)
    (ironscheme clr internal))
  
  (define-syntax clr-call
    (lambda (e)
      (syntax-case e ()
        [(_ target:member instance args ...)
         #'(clr-call-internal 'target:member instance args ...)])))

  (define-syntax clr-new
    (lambda (e)
      (syntax-case e ()
        [(_ type args ...)
         #'(clr-new-internal 'type args ...)])))
         
  (define-syntax clr-cast
    (lambda (e)
      (syntax-case e ()
        [(_ type arg)
         #'(clr-cast-internal 'type arg)])))         
         
)
    