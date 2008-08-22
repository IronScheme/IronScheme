(library (ironscheme web routing)
  (export
    process-request)
  (import 
    (ironscheme)
    (ironscheme strings)
    (ironscheme web))
    
  (define (parse-url)
    (let* ((f (vector-ref (string-split (request-raw-url) "?") 0))
           (t (string-split f "/")))
      (cdr (vector->list t))))

  (define-syntax match
    (lambda (x)
      (syntax-case x ()
        [(_ e ((m ...) f c c* ...) ...)
          #'(syntax-case e ()
              [(m ...)
                (apply (lambda (m ...) f) e)
                (apply (lambda (m ...) c c* ...) e)] ...
              [_ #f])])))
              
  (define (lookup c a)
    (let ((r (application-item 'routes)))
      (unless r
        (set! r (make-eq-hashtable))
        (application-item-set! 'routes r))
      (let ((ca (hashtable-ref r (string-append c ":" a) #f)))
        (or 
          ca
          (parameterize ((library-path (cons (map-path "~") (library-path))))
            (guard (e (#t (begin (printf "~s\n" e) #f)))
              (let ((ac (eval (string->symbol a) (environment (list 'controllers (string->symbol c))))))
                (printf "controller/action ~a/~a\n" c a)
                (hashtable-set! r (string-append c ":" a) ac)
                ac)))))))
          
  (define (load-controller/action c a)
    (let ((ca (lookup c a)))
      (if ca
        (ca)
        (display-html (format "No controller/action for ~a:~a" c a)))))
        
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
                #t
                (context-item-set! 'controller controller)
                (context-item-set! 'action "index")
                (load-controller/action controller "index")]
              [(controller action)
                #t
                (when (zero? (string-length action))
                  (set! action "index"))
                (context-item-set! 'controller controller)
                (context-item-set! 'action action)
                (load-controller/action controller action)]
              [(controller action id)
                #t
                (context-item-set! 'id id)
                (context-item-set! 'controller controller)
                (context-item-set! 'action action)
                (load-controller/action controller action)]))])))            
        
  (define (match-url url)
    (include-routes "web.routes"))           

  (define (process-request)    
    (match-url (parse-url)))
   
)
    
    
  
