(library (declare-type)
   (export local-declare-type local-declare-types)
   (import (rnrs) (only (ironscheme) gensym))

   (define-syntax identity
     (syntax-rules ()
       [(_ x) x]))
  
   (define-syntax local-declare-type
     (let ([mysecret (gensym)])
       (lambda (stx)
         (syntax-case stx ()
           [(_ (var pred?) e e* ...)
            (with-syntax ([(nocheck p-nocheck)
                           (let ([t0 (datum->syntax #'var mysecret)]
                                 [t1 (datum->syntax #'here mysecret)])
                             (if (free-identifier=? t0 t1)
                                 (list t0 #'identity)
                                 (list t0 t0)))])
              #'(let-syntax ([nocheck
                              (syntax-rules ()
                                [(_)   (p-nocheck var)]
                                [(_ x) (p-nocheck x)])])
                  (let-syntax ([var (identifier-syntax
                                      (if (pred? (nocheck))
                                          (nocheck)
                                          (assertion-violation 'var
                                            "type mismatch" 'pred? (nocheck))))])
                    (let-syntax ([nocheck
                                  (syntax-rules (var)
                                    [(_ var) (nocheck)]
                                    [(_ x)   (nocheck x)])])
                      e e* ...))))]))))

   (define-syntax local-declare-types
      (syntax-rules ()
        [(_ ((name1 expr1)) body1 body2 ...)
          (local-declare-type (name1 expr1)
            body1 body2 ...)]
        [(_ ((name1 expr1) (name2 expr2) ...) body1 body2 ...)
          (local-declare-type (name1 expr1)
            (local-declare-types ((name2 expr2) ...)
                body1 body2 ...))])))
