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
          (let ([fmt (fmt #'name)]
                [all-fields #'(fld ... dfld ...)])
           (with-syntax (((getter ...) (map (lambda (x) (fmt "~s-~s" #'name x)) all-fields))
                         ((setter ...) (map (lambda (x) (fmt "set-~s-~s!" #'name x)) all-fields))
                         ((afld ...) all-fields))
              #'(define-record-type name 
                  (fields
                    (mutable afld getter setter) ...)
                  (protocol
                    (lambda (p)
                      (lambda (fld ...)
                        (p fld ... val ...)))))))])))
           
  (define-syntax struct-case
    (lambda (x)
      (define (fmt ctxt)
        (lambda (str . args) 
          (datum->syntax ctxt 
            (string->symbol 
              (apply format str (map syntax->datum args))))))
      (syntax-case x (else)
        [(k r [(r? f ...) e e* ...] ... [else false-expr])
          (let ((fmt (fmt #'k)))
            (with-syntax (((pred? ...) (map (lambda (rn) 
                                              (fmt "~s?" rn)) 
                                            #'(r? ...)))
                          (((get ...) ...) (map (lambda (fn rn) 
                                                  (map (lambda (fn) 
                                                          (fmt "~s-~s" rn fn)) 
                                                       fn)) 
                                                #'((f ...) ...) 
                                                #'(r? ...))))
              #'(let ((r* r))
                  (cond
                    [(pred? r*)
                      (let ((f (get r*)) ...)
                        e e* ...)] ...
                    [else false-expr]))))]
        [(k r [(r? f ...) e e* ...] ...)
          #'(k r [(r? f ...) e e* ...] ... [else #f])])))

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