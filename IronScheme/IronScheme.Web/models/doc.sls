(library (models doc)
  (export
    get-library
    get-identifier
    save-library-description
    library-doc-description
    get-libraries
    get-symbols
    
    proc-id-doc
    identifier-doc)
  (import
    (ironscheme)
    (ironscheme web)
    (prefix (ironscheme web models) model:)
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
      (mutable forms)))

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
    (or (model:load-data "~/data/doc.data")
        (init-defaults)))
        
  (define (save-data)
    (model:save-data "~/data/doc.data" data))
          
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
          (match-type id lib)))
      #f))
      
  (define (proc-forms id lib)
    (let ((proc (eval id (environment `(only ,lib ,id)))))
    (call-with-values 
      (lambda ()
        (procedure-form proc))
      list)))
      
  (define (match-type id lib)
    (let ((t (get-id-type id lib)))
      (cond 
        [(enum-set=? t (make-id-type '(procedure)))
          (make-proc-id-doc id lib t "No description" #f (proc-forms id lib))]
        [else              
          (make-identifier-doc id lib t "No description" #f)])))
    
  (define (get-library lib)
    (update! 
      (get-lib-data) 
      (make-lib-sym lib) 
      (lambda (e) 
        (or e (make-library-doc lib "No description")))
      #f))
 
  (define (get-symbols lib sort)
    (iterator->list
      (from i in (environment-bindings (environment lib))
       orderby (case sort
                 [(type) (cdr i)]
                 [else (car i)])
       then (car i)
       ; the above scenario looks strange, but as the input is unique,
       ; the second call to (car i) never happens
       select i)))
     
  (define (make-lib-name lib)
    (let ((n (format "~a" lib)))
      (substring n 1 (- (string-length n) 1))))
      
  (define (make-lib-sym lib)
    (string->symbol (make-lib-name lib)))      
    
  (define (make-id-sym id lib)
    (string->symbol (string-append (symbol->string id) " " (make-lib-name lib))))        
     
  (define (get-libraries)
    (iterator->list   
      (from l in libraries
       select 
        (cons (car l) 
          (iterator->list 
            (from ll in (cdr l)
             orderby (make-lib-name ll)
             select ll))))))
     
  (define libraries
    '(("IronScheme"
      (ironscheme) 
      (ironscheme clr)
      (ironscheme console)
      (ironscheme contracts)
      (ironscheme conversions)
      (ironscheme datetime)
      (ironscheme define-macro)
      (ironscheme docs)
      (ironscheme environment)
      (ironscheme ffi)
      (ironscheme files)
      (ironscheme library-utils)
      (ironscheme linq)
      (ironscheme process)
      (ironscheme random)
      (ironscheme record-case)
      (ironscheme regex)
      (ironscheme registry)
      (ironscheme strings)
      (ironscheme symbolic-case)
      (ironscheme syntax-format)
      (ironscheme threading)
      (ironscheme web-utils)
      (ironscheme web)
      (ironscheme xml)
      (ironscheme clr reflection)
      (ironscheme web controllers)
      (ironscheme web views)
      (ironscheme web routing)
      (ironscheme web routing-helper)
      (ironscheme records printer)
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
      (srfi :0 cond-expand)
      (srfi :1 lists)
      (srfi :2 and-let*)
      (srfi :6 basic-string-ports)
      (srfi :8 receive)
      (srfi :9 records)
      (srfi :11 let-values)
      (srfi :13 strings)
      (srfi :14 char-sets)
      (srfi :16 case-lambda)
      (srfi :19 time)
      (srfi :23 error)
      (srfi :26 cut)
      (srfi :27 random-bits)
      (srfi :31 rec)
      (srfi :37 args-fold)
      (srfi :38 with-shared-structure)
      (srfi :39 parameters)
      (srfi :41 streams)
      (srfi :41 streams derived)
      (srfi :41 streams primitive)
      (srfi :42 eager-comprehensions)
      (srfi :43 vectors)
      (srfi :48 intermediate-format-strings)
      (srfi :61 cond)
      (srfi :64 testing)
      (srfi :67 compare-procedures)
      (srfi :78 lightweight-testing)
      (srfi :98 os-environment-variables)
      (srfi :99 records)
      (srfi :99 records inspection)
      (srfi :99 records procedural)
      (srfi :99 records syntactic))
    ("Other"
      (syn-param)
      (foof-loop)
      (as-match)
      (datatype)
      (declare-type)
      (draw-tree)
      (match)
      (list-match)
      (pregexp)
      (schelog)
      (tiny-talk))))         

)    