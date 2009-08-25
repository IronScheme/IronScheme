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