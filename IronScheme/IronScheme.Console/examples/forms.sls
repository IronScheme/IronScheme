(library (examples forms)
  (export
    application-exit
    application-run 
    make-form
    form-click-add!
    form-show)
  (import 
    (rnrs)
    (ironscheme clr))
  
  ;;will load assembly if not loaded
  (clr-reference System.Windows.Forms)
  (clr-using System.Windows.Forms)
  
  (define (make-form)
    (clr-new Form))
    
  (define (form-show form)
    (clr-call Form Show form))
    
  (define (application-run form)
    (clr-static-call Application (Run Form) form))
  
  (define (form-click-add! form proc)
    (clr-event-add! Form Click form proc))
    
  (define (application-exit)
    (clr-static-call Application Exit))
    

)

#| run with

(import (examples forms))
(define f (make-form))
(form-click-add! f (lambda (s a) (application-exit)))
(application-run f)

|#