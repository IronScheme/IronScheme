#| License
Copyright (c) 2022 Robby Zambito
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme async)
  (export async-lambda
   	  await
	  define-async
	  start
	  started?
	  status
	  task?)
  (import (ironscheme)
	  (ironscheme clr))

  (clr-using System.Threading.Tasks)
  (clr-using System.Runtime.CompilerServices)

  (define-syntax define-async
    (syntax-rules (->)
      ((_ (name params ...) -> ret-type body1 body2 ...)
       (define name
	 (async-lambda (params ...) -> ret-type
		       body1
		       body2 ...)))
      ((_ (name params ...) body1 body2 ...)
       (define name
	 (async-lambda (params ...)
		       body1
		       body2 ...)))))

  (define (started? task)
    ;; Created is the only status where it has not yet been started.
    (not (equal? (status task) 'Created)))

  (define (status task)
    (clr-prop-get Task Status task))

  ;; Construct a Task
  (define-syntax async-lambda
    (syntax-rules (->)
      ((_ (params ...) -> ret-type body1 body2 ...)
       (lambda (params ...)
	 (clr-new (System.Threading.Tasks.Task ret-type)
		  (lambda ()
		    body1
		    body2 ...))))
      ((_ (params ...) body1 body2 ...)
       (async-lambda (params ...) -> Object body1 body2 ...))))
  
  ;; Await the task and return the result.
  (define (await task)
    (start task)
    (clr-call (TaskAwaiter Object) GetResult (clr-call (Task Object) GetAwaiter task)))

  ;; Can be called any number of times without error, unlike Task.Start()
  ;; Returns the started task.
  (define (start task)
    (unless (started? task)
      (clr-call Task Start task))
    task)

  (define (task? obj)
    (clr-is Task obj)))
