#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

;; DO NOT PRECOMPILE THIS LIBRARY, IT HAS TO RUN IN A WEB CONTEXT
(library (ironscheme web routing)
  (export
    process-request)
  (import 
    (ironscheme)
    (ironscheme clr)
    (ironscheme strings)
    (ironscheme web)
    (ironscheme web routing-helper))
    
  (define (parse-url)
    (let* ((f (vector-ref (string-split (string-replace (request-raw-url) (request-app-path) "") "?") 0))
           (t (string-split f "/")))
      (cdr (vector->list t))))
             
  (define (lookup c a)
    (let ((r (application-item 'routes)))
      (unless r
        (set! r (make-eq-hashtable))
        (application-item-set! 'routes r))
      (let ((ca (hashtable-ref r (string-append c ":" a) #f)))
        (or 
          ca
          (parameterize ((library-path (cons (map-path "~") (library-path))))
            (guard (e (#t (begin (error-add! (clr-new IronScheme.Runtime.SchemeException e)) (printf "~s\n" e) #f)))
              (let ((ac (eval (string->symbol a) (environment (list 'controllers (string->symbol c))))))
                (printf "controller/action ~a/~a\n" c a)
                (hashtable-set! r (string-append c ":" a) ac)
                ac)))))))
          
  (define (load-controller/action c a)
    (let ((ca (lookup c a)))
      (if ca
        (begin
          (context-item-set! 'controller c)
          (context-item-set! 'action a)
          (ca)))))
        
  (define-syntax include-routes
    (lambda (x)
      (define (read-file filename)
        (if (file-exists? filename)
          (with-input-from-file filename
            (lambda ()
             (let f ()
               (let ((x (read)))
                 (if (eof-object? x) 
                     '()
                     (cons x (f)))))))
           '()))
      (syntax-case x ()
        [(k filename)
          (with-syntax (((r ...) (datum->syntax #'k 
                                  (read-file 
                                    (map-path 
                                      (syntax->datum #'filename)))))
                        (url (datum->syntax #'k 'url)))
          #'(match url 
              r ...
              [(controller)
                (positive? (string-length controller))
                (load-controller/action controller "index")]
              [(controller action)
                (load-controller/action controller (if (zero? (string-length action)) "index" action))]
              [(controller action id)
                (begin
                  (context-item-set! 'id (url-decode id))
                  (load-controller/action controller action))]))])))            
        
  (define (match-url url)
    (include-routes "web.routes"))           

  (define (process-request)    
    (match-url (parse-url))))
    
    
  
