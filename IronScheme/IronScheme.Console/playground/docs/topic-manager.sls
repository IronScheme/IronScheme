#| License
Copyright (c) 2007-2015 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme docs topic-manager)
  (export
    define-topic
    install-topic
    get-topic
    get-topic-id
    (rename (libraries help-libraries))
    )
  (import 
    (ironscheme)
    (ironscheme docs topic))
    
  (define libraries (make-eqv-hashtable))  
  (define topics (make-eqv-hashtable))
  (define id-map (make-eqv-hashtable))
 
  (define (install-topic t)
    (assert (topic? t))
    (let ((id   (topic-id t))
          (name (topic-name t)))
      (hashtable-set! id-map id name)
      (for-each 
        (lambda (lib)
          (hashtable-update! libraries lib 
            (lambda (k) 
              (append (list t) k)) 
            '()))
        (topic-library t))
      (hashtable-update! topics name 
        (lambda (k) 
          (append (list t) k)) 
        '())))
    
  (define (get-topic symbol)
    (hashtable-ref topics symbol '()))
    
  (define (get-topic-id symbol)
    (let ((t (get-topic symbol)))
      (if (null? t)
        symbol
        (topic-id (car t)))))
       
  (define-syntax define-topic
    (syntax-rules ()
      [(_ name e ...)
        (install-topic (make-topic '(name e ...)))]))        
)