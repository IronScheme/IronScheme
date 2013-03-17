#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

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
    (ironscheme)
    (ironscheme contracts)
    (ironscheme clr))
    
  (clr-using System.Threading)
  
  (define/contract (start-thread thread:thread)
    (clr-call Thread Start thread))
      
  (define (thread? obj)
    (clr-is Thread obj))
    
  (define/contract (make-thread proc:procedure)
    (let ((a (procedure-arity proc)))
      (unless (and (fixnum? a) (fx=? a 0))
        (assertion-violation 'make-thread "expected procedure with zero parameters" a proc)))
    (clr-new Thread (clr-cast ThreadStart proc)))
    
  (define/contract queue-work-item
    (case-lambda 
      [(proc)
        (queue-work-item proc #f)]
      [(proc:procedure state) 
        (clr-static-call ThreadPool QueueUserWorkItem proc state)]))
      
  (define/contract (thread-sleep dur:fixnum)
    (clr-static-call Thread Sleep (clr-cast Int32 dur)))      
   
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