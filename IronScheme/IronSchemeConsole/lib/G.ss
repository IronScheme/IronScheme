(library (G)
   (export local-declare-type)
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
                      e e* ...))))])))))