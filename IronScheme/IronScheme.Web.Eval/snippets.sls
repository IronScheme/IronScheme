(library (snippets)
  (export 
    load
    save)
  (import 
    (except (ironscheme) load)
    (ironscheme linq2)
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

  (define (load id) 
    (if id
        (let ((id (string->number id)))
          (if id
              (let ((e (find (lambda (e) (fx=? (snippet-id e) id)) entries)))
                (if e
                    (fprintf (http-output-port) "{ ~s: ~s }" "content" (snippet-content e))
                    (fprintf (http-output-port) "{ ~s: ~s }" "error" "not found")))
              (fprintf (http-output-port) "{ ~s: ~s }" "error" "id not number")))
        (fprintf (http-output-port) "{ ~s: ~s }" "error" "no id")))
  
  (define (save expr)
    
    (fprintf (http-output-port) "{ ~s: ~s }" "id" expr))
)  