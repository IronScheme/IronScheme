#| ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 2007,2008,2009
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************|#

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

#|
(define (make-type type . typeargs))

(define (make-typed-var name type))

(class-options
  public
  abstract
  sealed
  (extends baseclass)
  (implements interfaces ...))

(field-options
  public
  static
  internal
  protected
  new
  readonly
  volatile
  (type typename))
  
(event-options
  private
  internal
  static
  protected
  new
  (type typename))  

(property-options
  private
  internal
  static
  protected
  abstract
  virtual
  override
  (type typename))
  
(method-options
  private
  internal
  static
  protected
  abstract
  virtual
  override
  (returns typename))  
  
(constructor-options
  private
  internal
  static
  protected)   

(define-clr-class name
  
  (field name)
  (field name init)
  
  (event name)
  
  (property name)
  (property name (get body))
  (property name (set body))
  (property name (get body) (set body))
  
  (constructor (args) body)
  
  (method (name . args) body)
   
)  
  

|#
