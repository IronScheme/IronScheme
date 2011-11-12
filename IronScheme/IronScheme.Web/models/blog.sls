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
    (ironscheme web models)
    (ironscheme strings)
    (ironscheme linq))
    
  (define-record-type blog-entry 
    (fields id date author 
      (mutable subject)
      (mutable body)))
  
  (define (load-entries)  
    (or (load-data "~/data/blog.data")
        (list (make-blog-entry 1 (now) 
                'leppie "Welcome to the IronScheme blog" 
                "Use the username: admin and the password: admin to login") )))
        
  (define (save-entries)
    (save-data "~/data/blog.data" entries))   
  
  (define entries (load-entries))
  
  (define (get-next-id)
    (if (null? entries) 
      1
      (+ 1 (blog-entry-id (car entries)))))
  
  (define (add-entry subject body)
    (let ((e (make-blog-entry (get-next-id) (now) 
                (user-name) subject body)))
      (set! entries (cons e entries))
      (save-entries)
      e))
      
  (define (search-data t)
    (iterator->list 
      (from e in entries
       where (or (string-ci-contains? (blog-entry-subject e) t)
                 (string-ci-contains? (blog-entry-body e) t))
       select e)))
 
  (define (get-data page pagesize) 
    (let ((s (* page pagesize)))
      (iterator->list (take (skip entries s) pagesize))))
    
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
    (single/default 
      (from be in entries
       where (= (blog-entry-id be) id)
       select be)
      #f))
        
)
