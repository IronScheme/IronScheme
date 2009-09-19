(library (source-optimizer helpers)
  (export
    define-structure
    struct-case
    exp-case
    else)
  (import 
    (rnrs)
    (only (ironscheme) format)
    (source-optimizer match))
    
  (define-syntax define-structure
    (lambda (x)
      (define (fmt ctxt)
        (lambda (str . args) 
          (datum->syntax ctxt 
            (string->symbol 
              (apply format str (map syntax->datum args))))))    
      (syntax-case x ()
        [(_ (name fld ...))
          #'(define-structure (name fld ...) ())]
        [(_ (name fld ...)([dfld val] ...))
          (let ([fmt (fmt #'name)])
           (with-syntax (((getter ...) (map (lambda (x) (fmt "~s-~s" #'name x)) #'(fld ... dfld ...)))
                         ((setter ...) (map (lambda (x) (fmt "set-~s-~s!" #'name x)) #'(fld ... dfld ...)))
                         ((afld ...) #'(fld ... dfld ...)))
              #'(define-record-type name 
                  (fields
                    (mutable afld getter setter) ...)
                  (protocol
                    (lambda (p)
                      (lambda (fld ...)
                        (p fld ... val ...)))))))])))
                        
(define-syntax struct-case
  (lambda (x)
    (define (enumerate fld* i)
      (syntax-case fld* ()
        [() #'()]
        [(x . x*) 
         (with-syntax ([i i] [i* (enumerate #'x* (+ i 1))])
           #'(i . i*))]))
    (define (generate-body ctxt cls*)
      (syntax-case cls* (else)
        [() (with-syntax ([x x]) #'(error #f "unmatched " v 'x))]
        [([else b b* ...])  #'(begin b b* ...)]
        [([(rec-name rec-field* ...) b b* ...] . rest) (identifier? #'rec-name)
         (with-syntax ([altern (generate-body ctxt #'rest)]
                       [(id* ...) (enumerate #'(rec-field* ...) 0)]
                       [rtd #'(record-type-descriptor rec-name)])
          #'(if ((record-predicate rtd) v)
                (let ([rec-field* ((record-accessor rtd id*) v)] ...)
                  b b* ...)
                altern))]))
    (syntax-case x ()
      [(_ expr cls* ...)
       (with-syntax ([body (generate-body #'_ #'(cls* ...))])
         #'(let ([v expr]) body))])))                        
                    
  (define-syntax exp-case
    (lambda (x)
      (define (quote-head form)
        (syntax-case form ()
          [(head . rest)
            #'('head . rest)]))
      (syntax-case x (else)
        [(_ e [form expr] ... [else false-expr])
          (with-syntax (((form ...) (map quote-head #'(form ...))))
            #'(match e [form expr] ... [_ false-expr]))]
        [(_ e [form expr] ...)
          #'(exp-case e [form expr] ... [else #f])])))
          
   
)    