(library (models blog)
  (export
    search-data
    add-entry
    edit-entry!
    delete-entry!
    get-entry-id
    blog-entry-id
    blog-entry-date
    blog-entry-author
    blog-entry-subject
    blog-entry-body
    get-data)
  (import
    (ironscheme)
    (ironscheme datetime)
    (ironscheme web)
    (ironscheme strings)
    (ironscheme linq))
    
  (define-record-type blog-entry 
    (fields id date author 
      (mutable subject)
      (mutable body)))
  
  (define (load-entries)  
    (let ((fn (map-path "~/blog.data")))
      (if (file-exists? fn)
        (call-with-port (open-file-input-port fn)
          (lambda (p)
            (guard (e (#t (begin (delete-file fn) '())))
              (deserialize-port p))))
        (list (make-blog-entry 1 (now) 
                'leppie "Welcome to the IronScheme blog" 
                "Use the username: admin and the password: admin to login") ))))
        
  (define (save-entries)
    (let ((fn (map-path "~/blog.data")))
      (when (file-exists? fn) 
        (delete-file fn))
      (call-with-port (open-file-output-port fn)
        (lambda (p)
          (serialize-port entries p)))))      
  
  (define entries (load-entries))
  
  (define (get-next-id)
    (if (null? entries) 
      1
      (+ 1 (blog-entry-id (car entries)))))
  
  (define (add-entry subject body)
    (let ((e (make-blog-entry (get-next-id) (now) 
                (user-name) subject body)))
      (set! entries (cons e entries))
      (save-entries)))
      
  (define (search-data t)
    (let ((t (string-downcase t)))      
      (from e in entries
       where (or (string-contains? (string-downcase (blog-entry-subject e)) t)
                 (string-contains? (string-downcase (blog-entry-body e)) t))
       select e)))
 
  (define (get-data page pagesize) 
    (let ((s (* page pagesize)))
      (take (skip entries s) pagesize)))
    
  (define (delete-entry! id)
    (let ((e (get-entry-id id)))
      (set! entries (remq e entries))
      (save-entries)))
    
  (define (edit-entry! id subject body)
    (let ((e (get-entry-id id)))
      (blog-entry-subject-set! e subject)
      (blog-entry-body-set! e body)
      (save-entries)))   

  (define (get-entry-id id)
    (let ((q (from be in entries
              where (= (blog-entry-id be) id)
              select be)))
      (if (null? q)
        #f
        (car q))))
       
        
)
