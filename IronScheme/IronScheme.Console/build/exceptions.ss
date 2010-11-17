#| License
Copyright (c) 2007,2008,2009,2010 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme exceptions)
  (export
    with-exception-handler
    guard
    raise
    else
    =>
    assertion-violation
    error
    raise-continuable
    dynamic-wind)
  (import 
    (psyntax config) 
    (only (ironscheme) import make-stacktrace-condition stacktrace-condition? display-stacktrace)
    (ironscheme clr)
    (ironscheme unsafe)
    (ironscheme contracts)
    (except (rnrs) 
      with-exception-handler 
      raise 
      raise-continuable 
      assertion-violation 
      error
      dynamic-wind))
    
  (define/contract (dynamic-wind in:procedure proc:procedure out:procedure)
    (in)
    ($try/finally (proc)
                  (out)))      

  (define *current-exception-handlers*
    (list 
      (lambda (condition)
        (display "Unhandled exception:\n")
        (display condition)
        ;; let's get out of here        
        ($throw (clr-new IronScheme.Runtime.SchemeException condition)))))

  (define/contract (with-exception-handler handler:procedure thunk:procedure)
    (with-exception-handlers (cons handler *current-exception-handlers*)
                             thunk))

  (define (with-exception-handlers new-handlers thunk)
    (let ((previous-handlers *current-exception-handlers*))
      (dynamic-wind
        (lambda ()
          (set! *current-exception-handlers* new-handlers))
        thunk
        (lambda ()
          (set! *current-exception-handlers* previous-handlers)))))
          
  (define (add-stacktrace con)
    (if (or (not (display-stacktrace))
            (not (serious-condition? con))
            (not (condition? con))
            (non-continuable-violation? con)
            (stacktrace-condition? con))
        con
        (let ((cc (simple-conditions con)))
          (apply condition
                 (append cc
                        (list (make-stacktrace-condition
                                (clr-static-call IronScheme.Runtime.R6RS.Exceptions 
                                                 GetStackTrace))))))))

  (define (raise obj)
    (let ((obj (add-stacktrace obj)))
      (let ((handlers *current-exception-handlers*))
        (with-exception-handlers (cdr handlers)
          (lambda ()
            ((car handlers) obj)
            (raise
              (condition
                (make-non-continuable-violation)
                (make-message-condition "handler returned"))))))))

  (define (raise-continuable obj)
    (let ((obj (add-stacktrace obj)))
      (let ((handlers *current-exception-handlers*))
        (with-exception-handlers (cdr handlers)
          (lambda ()
            ((car handlers) obj))))))
          
  (define (assertion-violation who msg . irritants)
    (raise
      (condition
        (make-assertion-violation)
        (if who (make-who-condition who) (condition))
        (make-message-condition msg)
        (make-irritants-condition irritants))))

  (define (error who msg . irritants)
    (raise
      (condition
        (make-error)
        (if who (make-who-condition who) (condition))
        (make-message-condition msg)
        (make-irritants-condition irritants)))))