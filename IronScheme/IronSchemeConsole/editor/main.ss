(import 
  (ironscheme)
  (ironscheme clr)
  (ironscheme clr internal))
 
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
  

(define f (make-form))

(form-click-add! f (lambda (s a) (application-exit)))

(application-run f)


;; define a class... how do we do this?  
#|

(define-class name 
  (parent parentname)
  (implements interfaces ...)
  (attributes attr ...) ; like sealed protected public etc
  
  (members ...))
  
(define-syntax define-clr-class
  (lambda (e)
    (syntax-case e (parent implements attributes)
      [(_ name rest ...) #'(define-clr-class-internal 'name rest ...)])))  

|#

(define-syntax define-clr-class
  (lambda (e)
    (syntax-case e ()
      [(_ name rest ...) #'(define-clr-class-internal 'name rest ...)]))) 