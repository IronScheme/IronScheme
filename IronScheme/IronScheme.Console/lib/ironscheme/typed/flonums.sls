#| License
Copyright (c) 2007-2015 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme typed flonums)
  (export
    flonum?
    
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
    flexpt)
  (import
    (ironscheme clr)
    (ironscheme typed)
    (ironscheme unsafe)
    (ironscheme syntax utils)
    (ironscheme syntax shorthand)
    (except (ironscheme)
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
      fldiv
      fldiv0
      
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
      
      fl=?
      fl<?
      fl<=?
      fl>?
      fl>=?
      
      fl+
      fl-
      fl*
      fl/
      flnumerator
      fldenominator))

  (define-syntax define-fl
    (lambda (x)
      (define (pred? name)
        (clr-call String EndsWith (symbol->string (syntax->datum name)) "?"))
      (syntax-case x ()
        [(_ (name formals ...) body body* ...)
          (with-syntax (((type ...) (map (lambda (x) (datum->syntax x 'Double)) #'(formals ...)))
                        (ret-type (datum->syntax #'name (if (pred? #'name) 'Boolean 'Double))))
            #'(define: (name (formals : type) ... -> ret-type)
                body body* ...))])))

  (define-syntax-case (define-fl-comparer name)
    (with-syntax ((uname (syntax-format "$~a" #'name #'name)))
      #'(define name
          (case-lambda:
            [((x1 : Double) (x2 : Double) -> Boolean)
              (uname x1 x2)]))))

  (define-fl-comparer fl=?)
  (define-fl-comparer fl<?)
  (define-fl-comparer fl<=?)
  (define-fl-comparer fl>?)
  (define-fl-comparer fl>=?)

  (define-syntax-case (define-fl-binop0 name id)
    (with-syntax ((uname (syntax-format "$~a" #'name #'name)))
      #'(define name 
          (case-lambda:
            [(-> Double) id]
            [((x1 : Double) (x2 : Double) -> Double)
              (uname x1 x2)]))))

  (define-fl-binop0 fl+ 0.0)
  (define-fl-binop0 fl* 1.0)

  (define-fl (fldenominator fl)
    (if (or (flnan? fl) (flinfinite? fl))
        1.0
        (real->flonum (denominator fl))))

  (define-fl (flnumerator fl)
    (if (or (flnan? fl) (flinfinite? fl))
        fl
        (real->flonum (numerator fl))))

  (define fl-
    (case-lambda:
      [((x1 : Double) -> Double)
        ($fl- x1)]
      [((x1 : Double) (x2 : Double) -> Double)
        ($fl- x1 x2)]))

  (define fl/
    (case-lambda:
      [((x1 : Double) -> Double)
        ($fl/ 1.0 x1)]
      [((x1 : Double) (x2 : Double) -> Double)
        ($fl/ x1 x2)]))

  (define-fl (fldiv0 x1 x2)
    (let* ((d (fldiv x1 x2))
           (m (fl- x1 (fl* d x2))))
      (cond 
        [(fl<? m (flabs (fl/ x2 2.0))) d]
        [(fl<? 0.0 x2) (fl+ d 1.0)]
        [else (fl- d 1.0)])))

  (define-fl (flmod x1 x2)
    (fl- x1 (fl* (fldiv x1 x2) x2)))

  (define-fl (flmod0 x1 x2)
    (if (flinfinite? x2)
        +nan.0
        (fl- x1 (fl* (fldiv0 x1 x2) x2))))

  (define: (fldiv-and-mod (x1 : Double) (x2 : Double))
    (let ((d (fldiv x1 x2)))
      (values d (fl- x1 (fl* d x2))))) 

  (define-fl (fldiv x1 x2)
    (if (fl<? 0.0 x2)
      (flfloor (fl/ x1 x2))
      (fl- (flfloor (fl/ x1 (fl- x2))))))

  (define: (fldiv0-and-mod0 (x1 : Double) (x2 : Double))
    (let ((d (fldiv0 x1 x2)))
      (if (flinfinite? x2)
          (values d +nan.0)
          (values d (fl- x1 (fl* d x2))))))

  (define-fl (flinteger? fl)
    (fl=? 0.0 (flmod fl 1.0))) 

  (define-fl (flfinite? fl)
    (not (or (flinfinite? fl) (flnan? fl))))

  (define-fl (flinfinite? fl)
    (clr-static-call Double IsInfinity fl))

  (define-fl (flnan? fl)
    (clr-static-call Double IsNaN fl))

  (define-fl (flsin fl)
    (clr-static-call Math Sin fl))

  (define-fl (flcos fl)
    (clr-static-call Math Cos fl))

  (define-fl (fltan fl)
    (clr-static-call Math Tan fl))

  (define-fl (flasin fl)
    (clr-static-call Math Asin fl))

  (define-fl (flacos fl)
    (clr-static-call Math Acos fl))

  (define flatan 
    (case-lambda:
      [((fl : Double) -> Double)
        (clr-static-call Math Atan fl)]
      [((fl1 : Double) (fl2 : Double) -> Double) 
        (clr-static-call Math Atan2 fl1 fl2)]))

  (define fllog 
    (case-lambda:
      [((fl : Double) -> Double)
        (clr-static-call Math Log fl)]
      [((fl1 : Double) (fl2 : Double) -> Double) 
        (clr-static-call Math Log fl1 fl2)]))

  (define-fl (flsqrt fl)
    (clr-static-call Math Sqrt fl))
    
  (define-fl (flexp fl)
    (clr-static-call Math Exp fl))

  (define-fl (flexpt fl n)
    (clr-static-call Math Pow fl n))
    
  (define-fl (flceiling fl)
    (clr-static-call Math (Ceiling Double) fl))

  (define-fl (flfloor fl)
    (clr-static-call Math (Floor Double) fl))

  (define-fl (fltruncate fl)
    (clr-static-call Math (Truncate Double) fl))

  (define-fl (flround fl)
    (clr-static-call Math (Round Double) fl))

  (define-fl (flabs fl)
    (clr-static-call Math (Abs Double) fl))

  (define-fl (flpositive? r)
    (fl<? 0.0 r))

  (define-fl (flnegative? r)
    (fl>? 0.0 r))

  (define-fl (flzero? r)
    (fl=? 0.0 r))

  (define-fl (fleven? n)
    (fl=? 0.0 (flmod n 2.0)))

  (define-fl (flodd? n)
    (fl=? 1.0 (flmod n 2.0)))

  (define-fl (flmax a b)
    (if (fl<? a b) b a))

  (define-fl (flmin a b)
    (if (fl>? a b) b a)))