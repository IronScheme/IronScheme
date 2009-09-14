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

(library (ironscheme collections stack)
  (export
    make-stack
    stack?
    stack-clear!
    stack-contains?
    stack-peek
    stack-pop!
    stack-push!
    stack->vector
    stack-count
    stack-clone
    stack-isempty?)
  (import 
    (rnrs)
    (ironscheme clr))

  (clr-using System.Collections)
  
  (define (make-stack)
    (clr-new Stack))
    
  (define (stack? o)
    (clr-is Stack o))
  
  (define (stack-clear! s)
    (clr-call Stack Clear s))
  
  (define (stack-contains? s o)
    (clr-call Stack Contains s o))
    
  (define (stack-peek s)
    (clr-call Stack Peek s))
  
  (define (stack-pop! s)
    (clr-call Stack Pop s))
  
  (define (stack-push! s o)
    (clr-call Stack Push s o))
    
  (define (stack->vector s)
    (clr-call Stack ToArray s))
    
  (define (stack-clone s)
    (clr-call Stack Clone s))
    
  (define (stack-count s)
    (clr-prop-get Stack Count s))
  
  (define (stack-isempty? s)
    (= (stack-count s) 0))    

)