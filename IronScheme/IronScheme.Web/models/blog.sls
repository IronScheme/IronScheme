(library (models blog)
  (export
    add-entry
    get-entry-id
    blog-entry-date
    blog-entry-author
    blog-entry-subject
    blog-entry-body
    get-data)
  (import
    (ironscheme)
    (ironscheme datetime)
    (ironscheme web)
    (ironscheme linq))
    
  (define-record-type blog-entry (fields date author subject body))
  
  (define (load-entries)  
    (let ((fn (map-path "blog.data")))
      (if (file-exists? fn)
        (call-with-port (open-file-input-port fn)
          (lambda (p)
            (guard (e (#t '()))
              (deserialize-port p))))
        '())))
  
  (define entries (load-entries))
  
  (define (add-entry subject body)
    (let ((e (make-blog-entry (now) 'leppie subject body))
          (fn (map-path "blog.data")))
      (set! entries (cons e entries))
      (when (file-exists? fn) 
        (delete-file fn))
      (call-with-port (open-file-output-port fn)
        (lambda (p)
          (serialize-port entries p)))))
 
  (define (get-data)
    entries)   

  (define (get-entry-id id)
    (let ((q (from be in entries
              where (string-ci=? (blog-entry-subject be) id)
              select be)))
      (if (null? q)
        #f
        (car q))))
       
        
)
