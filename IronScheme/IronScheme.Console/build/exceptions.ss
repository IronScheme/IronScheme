#| License
Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
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
    dynamic-wind
    io-decoding-error
    io-encoding-error
    undefined-error
    lexical-error
    syntax-error
    file-not-found-violation
    i/o-port-violation
    file-already-exists-violation
    file-in-use-violation)
  (import 
    ;(psyntax config) 
    (only (ironscheme) import make-stacktrace-condition stacktrace-condition? display-stacktrace ungensym)
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
        (make-irritants-condition irritants))))
        
  (define (io-decoding-error)
    (raise (make-i/o-decoding-error #f)))
            
  (define (io-encoding-error)
    (raise (make-i/o-encoding-error #f #f)))
        
  (define (undefined-error sym)
    (raise 
      (condition
        (make-undefined-violation)
        (make-message-condition "attempted to use undefined symbol")
        (make-irritants-condition (list (ungensym sym))))))
        
  (define (lexical-error msg what)
    (raise 
      (condition
        (make-lexical-violation)
        (make-message-condition msg)
        (make-irritants-condition (list what)))))           

  (define (syntax-error who msg form subform)
    (raise 
      (condition
        (make-who-condition who)
        (make-message-condition msg)
        (make-syntax-violation form subform))))
        
  (define (file-not-found-violation who msg filename)
    (raise
      (condition
        (make-assertion-violation)
        (if who (make-who-condition who) (condition))
        (make-message-condition msg)
        (make-i/o-file-does-not-exist-error filename))))  

  (define (i/o-port-violation who msg port)
    (raise
      (condition
        (make-assertion-violation)
        (if who (make-who-condition who) (condition))
        (make-message-condition msg)
        (make-i/o-port-error port))))

  (define (file-already-exists-violation who filename)
    (raise
      (condition
        (if who (make-who-condition who) (condition))
        (make-i/o-file-already-exists-error filename))))
        
  (define (file-in-use-violation who filename)
    (raise
      (condition
        (if who (make-who-condition who) (condition))
        (make-i/o-file-protection-error filename))))
        
        
  )