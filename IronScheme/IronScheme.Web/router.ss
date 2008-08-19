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
          (guard (e (#t #f))
            (let ((ac (eval (string->symbol a) (environment (list 'controllers (string->symbol c))))))
              (hashtable-set! r (string-append c ":" a) ac)
              ac)))))))
        
(define (load-controller/action c a)
  (let ((ca (lookup c a)))
    (if ca
      (ca)
      (display-html (format "No controller/action for ~a:~a" c a)))))
      
(define-syntax include-routes
  (lambda (x)
    (syntax-case x ()
      [(id filename)
        #`(include-into id #,(map-path (syntax->datum #'filename)))])))            
      
(define (match-url url)
  (include-routes "routing.sls"))           
  
(match-url (parse-url))
