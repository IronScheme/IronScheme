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
  (clr-using system.windows.forms)
  
  (define (make-form)
    (clr-new form))
    
  (define (form-show form)
    (clr-call form show form))
    
  (define (application-run form)
    (clr-static-call application run (clr-cast form form)))
  
  (define (form-click-add! form proc)
    (clr-event-add! form click form proc))
    
  (define (application-exit)
    (clr-static-call application exit))
    

)

#| run with

(import (examples forms))
(define f (make-form))
(form-click-add! f (lambda (s a) (application-exit)))
(application-run f)

|#