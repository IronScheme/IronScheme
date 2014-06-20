#| License
Copyright (c) 2007-2014 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

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