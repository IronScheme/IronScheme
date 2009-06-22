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
    (ironscheme clr))
    
  (clr-using system.threading)
  
  (define (start-thread thread)
    (clr-call thread start thread))
      
  (define (thread? obj)
    (clr-is thread obj))
    
  (define (make-thread proc)
    (clr-new thread proc))  
    
  (define queue-work-item
    (case-lambda 
      [(proc)       (queue-work-item proc #f)]
      [(proc state) (clr-static-call threadpool queueuserworkitem proc state)]))
      
  (define (thread-sleep dur)
    (clr-static-call thread sleep (clr-cast system.int32 dur)))      
   
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