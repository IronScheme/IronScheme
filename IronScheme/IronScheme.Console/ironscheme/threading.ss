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

(library (ironscheme threading)
  (export
    make-thread
    thread?
    queue-work-item
    start-thread
    thread-sleep
    monitor-enter
    monitor-exit
    lock)
  (import 
    (rnrs)
    (ironscheme contracts)
    (ironscheme clr))
    
  (clr-using System.Threading)
  
  (define/contract (start-thread thread:thread)
    (clr-call Thread Start thread))
      
  (define (thread? obj)
    (clr-is Thread obj))
    
  (define/contract (make-thread proc:procedure)
    (clr-new Thread proc))
    
  (define queue-work-item
    (case/contract 
      [(proc:procedure)       (queue-work-item proc #f)]
      [(proc:procedure state) (clr-static-call Threadpool QueueUserWorkitem proc state)]))
      
  (define/contract (thread-sleep dur:fixnum)
    (clr-static-call Thread Sleep (clr-cast system.int32 dur)))      
   
  (define (monitor-enter obj)
    (clr-static-call Monitor Enter obj))
          
  (define (monitor-exit obj)
    (clr-static-call Monitor Exit obj))
    
  (define-syntax lock
    (lambda (x)
      (syntax-case x ()
        [(_ (obj) e e* ...)
          (identifier? #'obj)
          #'(if (null? obj)
              (assertion-violation 'lock "lock object cannot be null" 'obj)
              (let ((obj obj))
                (dynamic-wind
                  (lambda () (monitor-enter obj))
                  (lambda () e e* ...)
                  (lambda () (monitor-exit obj)))))])))
        
   
)