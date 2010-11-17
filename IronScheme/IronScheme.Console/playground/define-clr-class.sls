
#|
(library (ironscheme define-clr-class)
  (export define-clr-class)
  (import (rnrs))

  (define-syntax define-clr-class
     (lambda (x)
       (syntax-case x ()
         [(e ...)
          #'#f]
          )))
)


(define (make-type type . typeargs))

(define (make-typed-var name type))

(visibility-options
  public
  private
  internal
  protected)

(class-options
  public
  internal
  abstract
  sealed
  (extends baseclass)
  (implements interfaces ...))

(field-options
  (visibility-options)
  static
  readonly
  volatile
  (type typename))
  
(event-options
  (visibility-options)
  static
  (type typename))  

(property-options
  (visibility-options)
  static
  abstract
  virtual
  override
  (type typename))
  
(method-options
  (visibility-options)
  static
  abstract
  virtual
  override
  sealed
  (returns typename))  
  
(constructor-options
  (visibility-options)
  static)   

(define-clr-class name
  
  (field name)
  (field name init)
  
  (event name)
  
  (property name)
  (property name (get body))
  (property name (set body))
  (property name (get body) (set body)) ; value is implicit
  
  (constructor (args) body)
  
  (method (name . args) body)
   
)  
  

|#
