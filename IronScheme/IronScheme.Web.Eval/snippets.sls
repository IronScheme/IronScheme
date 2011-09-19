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
    (ironscheme strings)
    (ironscheme web)
    (ironscheme web models))
    
  (define-record-type snippet
    (fields id
            name
            content
            date
            tags
            (mutable views)))

  (define (save-entries)
    (save-data "~/snippets.data" entries))
    
  (define (load-entries)  
    (load-data "~/snippets.data"))
  
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
              (wprintf "{ ~s: ~s, ~s: ~s, ~s: ~s }" 
                       "id" (snippet-id e)
                       "name" (snippet-name e)
                       "content" (snippet-content e))
              (wprintf "{ ~s: ~s }" "error" "not found")))
        (wprintf "{ ~s: ~s }" "error" "no id")))
  
  (define (save name expr)
    (let ((name (string-trim name)))
      (if (fxzero? (string-length name))
          (wprintf "{ ~s: ~s }" "error" "empty name")
          (guard (e
              [e (wprintf "{ ~s: ~s }" "error" "invalid expression")])
            (let ((p (read (open-string-input-port (string-append "(begin " expr "\n)")))))
              (if (or (eof-object? p) (fx<=? (length p) 1))
                  (wprintf "{ ~s: ~s }" "error" "invalid expression")
                  (begin (core-expand p (interaction-environment))
                         (let* ((id (fx+ 1 (snippet-id (car entries))))
                                (e (make-snippet id name expr #f #f 0)))
                           (set! entries (cons e entries))
                           (save-entries)
                           (wprintf "{ ~s: ~s }" "id" id)))))))))
)  