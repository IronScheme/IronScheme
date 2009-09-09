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

(library (ironscheme exceptions)
  (export
    with-exception-handler
    guard
    raise
    else
    =>
    assertion-violation
    error
    raise-continuable)
  (import 
    (psyntax config) 
    (only (ironscheme) import make-stacktrace-condition stacktrace-condition? display-stacktrace)
    (ironscheme clr)
    (ironscheme unsafe)
    (except (rnrs) 
      with-exception-handler 
      raise 
      raise-continuable 
      assertion-violation 
      error))

  (define *current-exception-handlers*
    (list 
      (lambda (condition)
        (display "Unhandled exception:\n")
        (display condition)
        (newline)
        ($throw (clr-new Exception)))))

  (define (with-exception-handler handler thunk)
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