(library (ironscheme arithmetic flonums)
  (export
    flonum?
    real->flonum
    
    fl=?
    fl<?
    fl<=?
    fl>?
    fl>=?
    
    flinteger?
    flzero?
    flpositive?
    flnegative?
    flodd?
    fleven?
    flfinite?
    flinfinite?
    flnan?
    
    flmax
    flmin
    
    fl+
    fl*
    
    fl-
    fl/
    
    flabs
    
    fldiv-and-mod
    fldiv
    flmod
    fldiv0-and-mod0
    fldiv0
    flmod0
    
    flnumerator
    fldenominator
    
    flfloor
    flceiling
    fltruncate
    flround
    
    flexp
    fllog
    flsin
    flcos
    fltan
    flasin
    
    flacos
    flatan
    flsqrt
    flexpt

    fixnum->flonum)
    
  (import 
    (ironscheme clr)
    (except (rnrs)
    
    flinteger?
    flzero?
    flpositive?
    flnegative?
    flodd?
    fleven?  
    
    flfinite?
    flinfinite?
    flnan?    
    
    flmax
    flmin      
    
    flabs
    
    fldiv-and-mod
    flmod
    fldiv0-and-mod0
    flmod0    
    
    flfloor
    flceiling
    fltruncate
    flround
    
    flexp
    fllog
    flsin
    flcos
    fltan
    flasin
    
    flacos
    flatan
    flsqrt
    flexpt))
    
  (define (flmod x1 x2)
    (fl- x1 (fl* (fldiv x1 x2) x2)))

  (define (flmod0 x1 x2)
    (fl- x1 (fl* (fldiv0 x1 x2) x2)))
    
  (define (fldiv-and-mod x1 x2)
    (let ((d (fldiv x1 x2)))
      (values d (fl- x1 (fl* d x2)))))             

  (define (fldiv0-and-mod0 x1 x2)
    (let ((d (fldiv0 x1 x2)))
      (values d (fl- x1 (fl* d x2)))))       
   
  (define (flinteger? fl)
    (unless (flonum? fl)
      (assertion-violation 'flinteger? "not a flonum" fl))
    (fl=? 0.0 (flmod fl 1.0))) 
    
  (define (flfinite? fl)
    (unless (flonum? fl)
      (assertion-violation 'flfinite? "not a flonum" fl))
    (not (flinfinite? fl)))
    
  (define (flinfinite? fl)
    (unless (flonum? fl)
      (assertion-violation 'flinfinite? "not a flonum" fl))
    (clr-static-call system.double isinfinity fl))
    
  (define (flnan? fl)
    (unless (flonum? fl)
      (assertion-violation 'flnan? "not a flonum" fl))
    (clr-static-call system.double isnan fl))    
    
  (define (flsin fl)
    (unless (flonum? fl)
      (assertion-violation 'flsin "not a flonum" fl))
    (clr-static-call system.math sin fl))

  (define (flcos fl)
    (unless (flonum? fl)
      (assertion-violation 'flcos "not a flonum" fl))
    (clr-static-call system.math cos fl))

  (define (fltan fl)
    (unless (flonum? fl)
      (assertion-violation 'fltan "not a flonum" fl))
    (clr-static-call system.math tan fl))

  (define (flasin fl)
    (unless (flonum? fl)
      (assertion-violation 'flasin "not a flonum" fl))
    (clr-static-call system.math asin fl))

  (define (flacos fl)
    (unless (flonum? fl)
      (assertion-violation 'flacos "not a flonum" fl))
    (clr-static-call system.math acos fl))

  (define flatan 
    (case-lambda 
      [(fl)      (clr-static-call system.math atan fl)]
      [(fl1 fl2) (clr-static-call system.math atan2 fl1 fl2)]))

  (define fllog 
    (case-lambda
      [(fl)       (clr-static-call system.math log fl)]
      [(fl1 fl2)  (clr-static-call system.math log fl1 fl2)]))

  (define (flsqrt fl)
    (unless (flonum? fl)
      (assertion-violation 'flsqrt "not a flonum" fl))
    (clr-static-call system.math sqrt fl))
    
  (define (flexp fl)
    (unless (flonum? fl)
      (assertion-violation 'flexp "not a flonum" fl))
    (clr-static-call system.math exp fl))    

  (define (flexpt fl n)
    (unless (flonum? fl)
      (assertion-violation 'flexpt "not a flonum" fl))
    (clr-static-call system.math pow fl n))    
    
  (define (flceiling fl)
    (unless (flonum? fl)
      (assertion-violation 'flceiling "not a flonum" fl))
    (clr-static-call system.math "ceiling(double)" fl))    

  (define (flfloor fl)
    (unless (flonum? fl)
      (assertion-violation 'flfloor "not a flonum" fl))
    (clr-static-call system.math "floor(double)" fl))    

  (define (fltruncate fl)
    (unless (flonum? fl)
      (assertion-violation 'fltruncate "not a flonum" fl))
    (clr-static-call system.math "truncate(double)" fl))    

  (define (flround fl)
    (unless (flonum? fl)
      (assertion-violation 'flround "not a flonum" fl))
    (clr-static-call system.math "round(double)" fl))    
    
  (define (flabs fl)
    (unless (flonum? fl)
      (assertion-violation 'flabs "not a flonum" fl))
    (clr-static-call system.math "abs(double)" fl)) 
    
    
  (define (flpositive? r)
    (unless (flonum? r)
      (assertion-violation 'flpositive? "not a flonum" r))
    (fl<? 0.0 r))
    
  (define (flnegative? r)
    (unless (flonum? r)
      (assertion-violation 'flnegative? "not a flonum" r))
    (fl>? 0.0 r))   
    
  (define (flzero? r)
    (unless (flonum? r)
      (assertion-violation 'flzero? "not a flonum" r))
    (fl=? 0.0 r))           
    
  (define (fleven? n)
    (unless (integer-valued? n)
      (assertion-violation 'fleven? "not integer valued" n))
    (fl=? 0.0 (flmod n 2.0)))           

  (define (flodd? n)
    (unless (integer-valued? n)
      (assertion-violation 'flodd? "not integer valued" n))
    (fl=? 1.0 (flmod n 2.0)))      
  
  (define (flmax a . rest)
    (unless (flonum? a)
      (assertion-violation 'flmax "not a flonum" a))
    (fold-left 
      (lambda (a b) 
        (if (fl<? a b) b a))
      a 
      rest))
    
  (define (flmin a . rest)
    (unless (flonum? a)
      (assertion-violation 'flmin "not a flonum" a))
    (fold-left 
      (lambda (a b) 
        (if (fl>? a b) b a))
      a 
      rest))     

    
)