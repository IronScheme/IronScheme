#| License
Copyright (c) 2007,2008,2009,2010 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme docs)
  (export 
    help 
    define-topic 
    get-topic 
    help-libraries 
    topic->maml)
  (import 
    (ironscheme)
    (ironscheme docs topic)
    (ironscheme docs topic-manager)
		(ironscheme docs topic-printer)
    (ironscheme docs templates))
    
  (define (print-help t)
    (if (null? t)
      (display "no help\n")
      (for-each print-topic t)))
  
  (define-syntax help
    (syntax-rules () 
      [(_ s)
        (let ((t (get-topic 's)))
          (print-help t))]
      [(_ s lib)
        (let ((t (filter 
										(lambda (t) 
											(member 'lib (topic-library t))) 
										(get-topic 's))))
          (print-help t))]))
    
)  

#|

(import (ironscheme docs))

(define-topic car                                 ; name
  (description "Gets the car element of a list")  ; optional
  (form (car list))                               ; required
  (param list "a list")                           ; zero or more
  (return "the car of the list")                  ; required
  (exception &assertion "if list not a list")     ; zero or more
  (remark "remarks")                              ; optional
  (example "description"  (car '(1 2)) => 1)      ; zero or more examples
  (library (rnrs) (rnrs base))                    ; required
  (related car cons))
  
(define-topic cdr
  (description "Gets the cdr element of a list")
  (form (cdr list))
  (param list "a list")
  (return "the cdr of the list")
  (exception &assertion "if list not a list")
  (example (cdr '(1 2)) => (2))
  (example (cdr '(1 . 2)) => 2)
  (library (rnrs))
  (related car))
  
(help car)

(display (topic->maml (car (get-topic 'car))))

|#
