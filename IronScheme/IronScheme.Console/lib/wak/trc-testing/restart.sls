#!r6rs
;;;; Testing Utility for Scheme
;;;; R6RS Restart Kludgery

;;; Adaption for R6RS by Andreas Rottmann, changes licensed under the
;;; new-style BSD license (see COPYING.BSD in the top-level
;;; directory).

;;; Originally written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(library (wak trc-testing restart)
  (export
   make-restarter
   restarter?
   restarter-tag
   restarter-description
   restart
   restart-interactively
   current-restarters
   with-restarter
   find-restarter
   call-with-restarter
   call-with-interactive-restarter
   with-exiting-restarter
   call-with-exiting-restarter

   ;; originally in another structure (restarting-hooks)
   with-restarter-invoker-hook
   )
  (import (rnrs)
          (srfi :8 receive)
          (srfi :39 parameters))

(define-record-type restarter
  (fields tag description invoker interactor)
  (protocol
   (lambda (p)
     (lambda (tag description invoker interactor)
       (p tag description ((restarter-invoker-hook) invoker) interactor)))))

;++ Bletch!  This is an outrageous crock.

(define restarter-invoker-hook (make-parameter values))

(define (with-restarter-invoker-hook hook thunk)
  (parameterize ((restarter-invoker-hook hook))
    (thunk)))

; (put 'with-restarter-invoker-hook 'scheme-indent-function 1)

(define (continuable-error who message . irritants)
  (raise-continuable (condition
                      (make-error)
                      (make-who-condition who)
                      (make-message-condition message)
                      (make-irritants-condition irritants))))

(define (restart spec . args)
  (let ((win (lambda (r) (apply (restarter-invoker r) args))))
    (cond ((restarter? spec)
           (win spec))
          ((find-restarter spec)
           => win)
          (else
           (apply restart
                  (continuable-error 'restart
                                     "invalid restarter specifier"
                                     `(restart ,spec ,@args))
                  args)))))

(define (restart-interactively spec)
  (let ((win (lambda (r)
               (receive args ((restarter-interactor r))
                 (apply (restarter-invoker r) args)))))
    (cond ((restarter? spec)
           (win spec))
          ((find-restarter spec)
           => win)
          (else
           (restart-interactively
            (continuable-error 'restart-interactively
                               "invalid restarter specifier"
                               `(restart-interactively ,spec)))))))

(define $restarters (make-parameter '()))

(define (current-restarters) ($restarters))

(define (with-restarter restarter thunk)
  (parameterize (($restarters (cons restarter ($restarters))))
    (thunk)))

; (put 'with-restarter 'scheme-indent-function 1)

(define (find-restarter tag . restarters)
  (let loop ((restarters (if (pair? restarters)
                             (car restarters)
                             (current-restarters))))
    (cond ((null? restarters)
           #f)
          ((eqv? (restarter-tag (car restarters)) tag)
           (car restarters))
          (else
           (loop (cdr restarters))))))

(define (call-with-restarter tag description invoker receiver)
  (call-with-interactive-restarter tag description invoker #f
    receiver))

(define (call-with-interactive-restarter tag description
            invoker interactor
          receiver)
  (let ((restarter
         (make-restarter tag description invoker interactor)))
    (with-restarter restarter
      (lambda ()
        (receiver restarter)))))

; (put 'call-with-restarter 'scheme-indent-functioon 3)
; (put 'call-with-interactive-restarter 'scheme-indent-function 4)

(define (with-exiting-restarter tag description thunk)
  (call-with-exiting-restarter tag description
    (lambda (r) (thunk))))

(define (call-with-exiting-restarter tag description receiver)
  (call-with-current-continuation
    (lambda (exit)
      (call-with-interactive-restarter
          tag description
          (lambda () (exit))            ; invoker
          (lambda () (values))          ; interactor
        receiver))))

; (put 'with-exiting-restarter 'scheme-indent-function 2)
; (put 'call-with-exiting-restarter 'scheme-indent-function 2)

  )
