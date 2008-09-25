(library (models doc)
  (export
    get-library
    get-identifier
    save-library-description
    library-doc-description
    get-libraries
    get-symbols)
  (import
    (ironscheme)
    (ironscheme web)
    (ironscheme strings)
    (ironscheme linq))

  (define-enumeration identifier-type 
    (variable 
     procedure 
     parameter 
     syntax 
     aux-syntax 
     record 
     condition 
     unknown) id-type)    
  
  (define-record-type library-doc 
    (fields 
      name 
      (mutable description)))
  
  (define-record-type identifier-doc 
    (fields 
      name 
      lib 
      (mutable type) 
      (mutable description) 
      (mutable example)))
  
  (define-record-type syntax-id-doc 
    (parent identifier-doc) 
    (fields 
      (mutable aux-ids)
      (mutable forms)))
      
  (define-record-type aux-syntax-id-doc
    (parent identifier-doc)
    (fields
      (mutable primary)))
      
  (define-record-type proc-id-doc  
    (parent identifier-doc)
    (fields
      (mutable parameters)))

  (define-record-type record-id-doc (parent identifier-doc))
  (define-record-type condition-id-doc (parent identifier-doc))
  
  (define make-id-type (enum-set-constructor (id-type)))
  
  (define (update! ht key proc default)
    (let* ((e (hashtable-ref ht key default))
           (e* (proc e)))
      (if (eq? e e*) 
        e
        (begin
          (hashtable-set! ht key e*)
          e*))))  
  
  (define (init-defaults)
    (make-eq-hashtable))
  
  (define (load-data)  
    (let ((fn (map-path "~/data/doc.data")))
      (if (file-exists? fn)
        (call-with-port (open-file-input-port fn)
          (lambda (p)
            (guard (e (#t (begin (delete-file fn) (init-defaults))))
              (deserialize-port p))))
        (init-defaults))))
        
  (define (save-data)
    (let ((fn (map-path "~/data/doc.data")))
      (when (file-exists? fn) 
        (delete-file fn))
      (call-with-port (open-file-output-port fn)
        (lambda (p)
          (serialize-port data p)))))    
          
  (define data (load-data))    
  
  (define (get-lib-data)
    (update! 
      data 
      'libraries 
      (lambda (e)
        (or e (make-eq-hashtable)))
      #f))
      
  (define (get-id-data)
    (update! 
      data 
      'identifiers 
      (lambda (e)
        (or e (make-eq-hashtable)))
      #f))      
      
  (define (save-library-description lib desc)
    (let ((ld (get-library lib)))
      (library-doc-description-set! ld desc)
      (save-data)))
      
  (define (get-id-type id lib)
    (let ((t (single/default 
              (from i in (environment-bindings (environment lib))
               where (eq? (car i) id)
               select (cdr i))
              #f)))
      (make-id-type (list (if t t 'unknown)))))
      
  (define (get-identifier id lib)
    (update! 
      (get-id-data) 
      (make-id-sym id lib) 
      (lambda (e) 
        (or 
          e 
          (make-identifier-doc 
            id 
            lib 
            (get-id-type id lib)
            "No description"
            #f)))
      #f))          
    
  (define (get-library lib)
    (update! 
      (get-lib-data) 
      (make-lib-sym lib) 
      (lambda (e) 
        (or e (make-library-doc lib "No description")))
      #f))
 
  (define (get-symbols lib sort)
    (from i in (environment-bindings (environment lib))
     orderby (case sort
               [(type) (cdr i)]
               [else (car i)])
     then (car i)
     ; the above scenario looks strange, but as the input is unique,
     ; the second call to (car i) never happens
     select i))
     
  (define (make-lib-name lib)
    (let ((n (format "~a" lib)))
      (substring n 1 (- (string-length n) 1))))
      
  (define (make-lib-sym lib)
    (string->symbol (make-lib-name lib)))      
    
  (define (make-id-sym id lib)
    (string->symbol (string-append (symbol->string id) " " (make-lib-name lib))))        
     
  (define (get-libraries) 
    (from l in libraries
     select 
      (cons (car l) 
        (from ll in (cdr l)
         orderby (make-lib-name ll)
         select ll))))
     
  (define libraries
    '(("IronScheme"
      (ironscheme) 
      (ironscheme clr)
      (ironscheme console)
      (ironscheme conversions)
      (ironscheme datetime)
      (ironscheme define-macro)
      (ironscheme docs)
      (ironscheme environment)
      (ironscheme files)
      (ironscheme linq)
      (ironscheme pretty-print)
      (ironscheme process)
      (ironscheme random)
      (ironscheme record-case)
      (ironscheme regex)
      (ironscheme registry)
      (ironscheme strings)
      (ironscheme syntax-format)
      (ironscheme threading)
      (ironscheme typed)
      (ironscheme web-utils)
      (ironscheme web)
      (ironscheme xml)
      (ironscheme web controllers)
      (ironscheme web views)
      (ironscheme web routing)
      (ironscheme web routing-helper)
      (ironscheme collections arraylist)
      (ironscheme collections icollection)
      (ironscheme collections ilist)
      (ironscheme collections stack))
    ("R6RS"
      (rnrs)
      (rnrs base)
      (rnrs r5rs)
      (rnrs control)
      (rnrs eval)
      (rnrs mutable-pairs)
      (rnrs mutable-strings)
      (rnrs programs)
      (rnrs syntax-case)
      (rnrs files)
      (rnrs sorting)
      (rnrs lists)
      (rnrs io simple)
      (rnrs bytevectors)
      (rnrs unicode)
      (rnrs exceptions)
      (rnrs arithmetic bitwise)
      (rnrs arithmetic fixnums)
      (rnrs arithmetic flonums)
      (rnrs hashtables)
      (rnrs io ports)
      (rnrs enums)
      (rnrs conditions)
      (rnrs records inspection)
      (rnrs records procedural)
      (rnrs records syntactic))
    ("SRFI"
      (srfi and-let)
      (srfi format)
      (srfi land)
      (srfi lists)
      (srfi parameters)
      (srfi receive)
      (srfi streams)
      (srfi string-ports)
      (srfi system))
    ("Other"
      (datatype)
      (declare-type)
      (draw-tree)
      (match)
      (pregexp)
      (schelog)
      (tiny-talk))))         

)    