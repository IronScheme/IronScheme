(library (snippets)
  (export 
    load
    save
    get-snippets
    snippet-name
    snippet-id
    load-snippet-content)
  (import 
    (except (ironscheme) load)
    (ironscheme linq2)
    (ironscheme strings)
    (ironscheme web))
    
  (define-record-type snippet
    (fields id
            name
            content
            date
            tags
            (mutable views)))

  (define (save-entries)
    (let ((fn (map-path "~/snippets.data")))
      (delete-file fn)
      (call-with-port (open-file-output-port fn)
        (lambda (p)
          (serialize-port entries p)))))
    
  (define (load-entries)  
    (let ((fn (map-path "~/snippets.data")))
      (if (file-exists? fn)
        (call-with-port (open-file-input-port fn)
          (lambda (p)
            (guard (e (#t (begin (delete-file fn) #f)))
              (deserialize-port p))))
        #f)))
  
  (define entries (or (load-entries)
                      (list (make-snippet 1 "First one" "(display 'HelloWorld)" #f #f 0))))
                      
  (define (get-snippets)
    (list-sort (lambda (a b)
                 (string-ci<?
                   (snippet-name a)
                   (snippet-name b)))
               entries))
    
  (define (load-snippet id)
    (let ((id (string->number id)))
      (and id
           (find (lambda (e) (fx=? (snippet-id e) id)) entries))))
           
  (define (load-snippet-content id)
    (let ((e (load-snippet id)))
      (and e
           (snippet-content e))))

  (define (load id) 
    (if id
        (let ((e (load-snippet id)))
          (if e
              (fprintf (http-output-port) "{ ~s: ~s, ~s: ~s, ~s: ~s }" 
                       "id" (snippet-id e)
                       "name" (snippet-name e)
                       "content" (snippet-content e))
              (fprintf (http-output-port) "{ ~s: ~s }" "error" "not found")))
        (fprintf (http-output-port) "{ ~s: ~s }" "error" "no id")))
  
  (define (save name expr)
    (let ((name (string-trim name)))
      (if (fxzero? (string-length name))
          (fprintf (http-output-port) "{ ~s: ~s }" "error" "empty name")
          (guard (e
              [e (fprintf (http-output-port) "{ ~s: ~s }" "error" "invalid expression")])
            (let ((p (read (open-string-input-port (string-append "(begin " expr ")")))))
              (if (eof-object? p)
                  (fprintf (http-output-port) "{ ~s: ~s }" "error" "invalid expression")
                  (begin (core-expand p (interaction-environment))
                         (let* ((id (fx+ 1 (snippet-id (car entries))))
                                (e (make-snippet id name expr #f #f 0)))
                           (set! entries (cons e entries))
                           (save-entries)
                           (fprintf (http-output-port) "{ ~s: ~s }" "id" id)))))))))
)  