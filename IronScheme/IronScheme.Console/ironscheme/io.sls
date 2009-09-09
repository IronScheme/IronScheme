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

(library (ironscheme io)
  (export
    null-textual-output-port
    null-textual-input-port
    null-binary-port
    
    synchronized-textual-output-port
    synchronized-textual-input-port
    synchronized-binary-port
    )
    
  (import 
    (ironscheme)
    (ironscheme contracts)
    (ironscheme clr))
    
  (clr-using System.IO)
 
  (define (null-textual-output-port)
    (clr-static-field-get TextWriter Null))    

  (define (null-textual-input-port)
    (clr-static-field-get TextReader Null))    

  (define (null-binary-port)
    (clr-static-field-get Stream Null))    

  (define/contract (synchronized-textual-output-port port:textual-output-port)
    (clr-static-call TextWriter Synchronized port))    

  (define/contract (synchronized-textual-input-port port:textual-input-port)
    (clr-static-call TextReader Synchronized port))    

  (define/contract (synchronized-binary-port port:binary-port)
    (clr-static-call Stream Synchronized port))    
    
)