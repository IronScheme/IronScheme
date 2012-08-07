#| License
Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme numbers)
  (export
    =
    <
    >
    <=
    >=
    zero?
    positive?
    negative?
    complex?
    real?
    rational?
    integer?
    real-valued?
    rational-valued?
    integer-valued?    
    exact?
    inexact?
    make-polar
    make-rectangular
    angle
    magnitude
    finite?
    exact-integer?
    numerator
    denominator
    imag-part
    real-part
    nan?
    infinite?
    exp
    sin
    asin
    sinh
    cos
    acos
    cosh
    tan
    tanh
    log
    atan
    div
    abs
    floor
    ceiling
    truncate
    round
    fixnum->flonum
    real->flonum
    inexact
    exact
    sqrt
    exact-integer-sqrt
    expt
    rationalize
    even?
    odd?
    max
    min
    gcd
    lcm
    div0
    mod
    mod0
    div-and-mod
    div0-and-mod0    
    number->string
    fx+/carry
    fx-/carry
    fx*/carry
    )
  (import 
    (except 
      (ironscheme)
      =
      <
      >
      <=
      >=
      zero?
      positive?
      negative?
      complex?
      real?
      rational?
      integer?
      real-valued?
      rational-valued?
      integer-valued?    
      exact?
      inexact?
      make-polar
      make-rectangular
      angle
      magnitude
      finite?
      numerator
      denominator
      imag-part
      real-part
      nan?
      infinite?
      exp
      sin
      asin
      sinh
      cos
      acos
      cosh
      tan
      tanh
      log
      atan
      div
      abs
      floor
      ceiling
      truncate
      round
      fixnum->flonum
      real->flonum
      inexact
      exact
      sqrt
      exact-integer-sqrt
      expt
      rationalize
      even?
      odd?
      max
      min
      gcd
      lcm
      div0
      mod
      mod0
      div-and-mod
      div0-and-mod0       
      number->string
      fx+/carry
      fx-/carry
      fx*/carry)
    (ironscheme core)
    (ironscheme contracts)
    (ironscheme unsafe)
    (ironscheme clr))
    
  (clr-using Oyster.Math)
  (clr-using Microsoft.Scripting.Math)
  (clr-using IronScheme.Runtime)    
    
  (define (make-rectnum r1 r2)
    (clr-static-call ComplexFraction Make r1 r2))
    
  (define (rectnum-imag-part c)
    (clr-prop-get ComplexFraction Imag c))
    
  (define (rectnum-real-part c)
    (clr-prop-get ComplexFraction Real c))    
    
  (define (ratnum-denominator rat)
    (clr-prop-get Fraction Denominator rat))   
    
  (define (ratnum-numerator rat)
    (clr-prop-get Fraction Numerator rat))  
  
  (define (make-complexnum r1 r2)
    (clr-static-call Complex64 Make r1 r2))
    
  (define (complexnum-imag-part c)
    (clr-prop-get Complex64 Imag c))
    
  (define (complexnum-real-part c)
    (clr-prop-get Complex64 Real c))
    
  (define (bignum/ a b)
    (clr-static-call IntX op_Division a b))    
    
  (define (bignum% a b)
    (clr-static-call IntX op_Modulus a b))
    
  (define (bignum->fixnum b)
    (clr-call IntX ToInt32 b))
    
  (define (flonum->ratnum f)
    (clr-static-call Fraction (op_Implicit Double) f))

  (define (ratnum->flonum r)
    (clr-call Fraction (ToDouble IFormatProvider) r '()))
    
  (define (fixnum->bignum f)
    (clr-static-call IntX (Create Int32) f))   
    
  (define (real->complexnum num)
    (if (complexnum? num)
        num
        (make-complexnum (inexact num) 0.0)))
        
  (define (complexnum->rectnum num)
    (clr-static-call ComplexFraction (op_Implicit Complex64) num))     
    
  (define (rectnum->complexnum num)
    (clr-call ComplexFraction ToComplex64 num))               
        
  (define (->fixnum num)
    (if (fixnum? num)
        num
        (clr-call IntX ToInt32 num)))
    
  (define (->ratnum num)
    (cond 
      [(ratnum? num) num]
      [(real? num)
        (flonum->ratnum (real->flonum num))]
      [else
        (assertion-violation '->ratnum "not a real" num)]))
        
  (define (->bignum num)
    (cond 
      [(bignum? num) num]
      [(fixnum? num) (fixnum->bignum num)]
      [else
        (assertion-violation '->bignum "not an integer" num)]))     

  (define (real->flonum x)
    (cond
      [(flonum? x)
        x]
      [(fixnum? x)
        (fixnum->flonum x)]
      [(bignum? x)
        (clr-call IntX ToFloat64 x)]
      [(real? x)
        (clr-static-call Convert (ToDouble Object) x)]
      [else
        (assertion-violation 'real->flonum "not a real" x)]))
    
  (define (fixnum->flonum x)
    (unless (fixnum? x)
      (assertion-violation 'fixnum->flonum "not a fixnum" x))
    (clr-cast Double (clr-cast Int32 x)))
        
  (define (nan? num)
    (cond
      [(or (fixnum? num)
           (bignum? num)
           (ratnum? num)
           (rectnum? num))
        #f]
      [(flonum? num)
        (flnan? num)]
      [(complexnum? num)
        (or (flnan? (complexnum-real-part num))
            (flnan? (complexnum-imag-part num)))]
      [else
        (assertion-violation 'nan? "not a number" num)]))
        
  (define (finite? num)
    (cond
      [(or (fixnum? num)
           (bignum? num)
           (ratnum? num)
           (rectnum? num))
        #t]
      [(flonum? num)
        (flfinite? num)]
      [(complexnum? num)
        (and (flfinite? (complexnum-real-part num))
             (flfinite? (complexnum-imag-part num)))]
      [else
        (assertion-violation 'finite? "not a number" num)]))        
        
  (define (infinite? num)
    (cond
      [(or (fixnum? num)
           (bignum? num)
           (ratnum? num)
           (rectnum? num))
        #f]
      [(flonum? num)
        (flinfinite? num)]
      [(complexnum? num)
        (or (flinfinite? (complexnum-real-part num))
            (flinfinite? (complexnum-imag-part num)))]
      [else
        (assertion-violation 'infinite? "not a number" num)]))
        
  (define (exact? obj)
    (cond
      [(or (fixnum? obj) 
           (bignum? obj)
           (ratnum? obj)
           (rectnum? obj))
       #t]
      [(or (flonum? obj)
           (complexnum? obj))
       #f]
      [else
        (assertion-violation 'exact "not a number" obj)]))

  (define (inexact? obj)
    (cond
      [(or (fixnum? obj) 
           (bignum? obj)
           (ratnum? obj)
           (rectnum? obj))
       #f]
      [(or (flonum? obj)
           (complexnum? obj))
       #t]
      [else
        (assertion-violation 'inexact "not a number" obj)]))
        
  (define (complex? obj)
    (number? obj))
    
  (define (real? obj)
    (cond
      [(or (fixnum? obj) 
           (bignum? obj)
           (ratnum? obj)
           (flonum? obj))
       #t]
      [(or (complexnum? obj) (rectnum? obj))
        (let ((i (imag-part obj)))
         (and (zero? i)
              (exact? i)))]
      [else #f]))
        
  (define (rational? obj)
    (cond
      [(or (fixnum? obj) 
           (bignum? obj)
           (ratnum? obj))
       #t]
      [(and (or (complexnum? obj) 
                (rectnum? obj)
                (flonum? obj)) 
            (finite? obj) 
            (not (nan? obj)))
        (let ((i (imag-part obj)))
          (and (exact? i) 
               (zero? i)))]
      [else #f]))
        
  (define (integer? obj)
    (cond
      [(or (fixnum? obj) 
           (bignum? obj))
       #t]
      [(and (or (ratnum? obj) 
                (complexnum? obj) 
                (rectnum? obj)
                (flonum? obj))
            (finite? obj) 
            (not (nan? obj)))            
        (let ((i (imag-part obj)))
          (and (exact? i) 
               (zero? i)
               (= (denominator (real-part obj)) 1)))]
      [else #f]))
      
  (define (real-valued? obj)
    (cond
      [(or (fixnum? obj) 
           (bignum? obj)
           (ratnum? obj)
           (flonum? obj))
       #t]
      [(or (complexnum? obj) (rectnum? obj))
        (let ((i (imag-part obj)))
          (zero? i))]
      [else #f])) 
      
  (define (rational-valued? obj)
    (cond
      [(or (fixnum? obj) 
           (bignum? obj)
           (ratnum? obj))
       #t]
      [(and (or (complexnum? obj) 
                (rectnum? obj)
                (flonum? obj)) 
            (finite? obj) 
            (not (nan? obj)))
        (let ((i (imag-part obj)))
          (zero? i))]
      [else #f])) 
      
  (define (integer-valued? obj)
    (cond
      [(or (fixnum? obj) 
           (bignum? obj))
       #t]
      [(and (or (ratnum? obj) 
                (complexnum? obj) 
                (rectnum? obj)
                (flonum? obj))
            (finite? obj) 
            (not (nan? obj)))            
        (let ((i (imag-part obj)))
          (and (zero? i)
               (= (denominator (real-part obj)) 1)))]
      [else #f]))                

  (define/contract (zero? num:number)
    (= num 0))
    
  (define/contract (positive? num:number)
    (> num 0))
      
  (define/contract (negative? num:number)
    (< num 0))
    
  (define (inexact num)
    (cond
      [(or (complexnum? num) 
           (flonum? num)) 
         num]
      [(or (exact-integer? num)
           (ratnum? num))
         (real->flonum num)]
      [(rectnum? num)
        (rectnum->complexnum num)]
      [else
        (assertion-violation 'inexact "not a number" num)]))
        
  (define (exact num)
    (cond
      [(complexnum? num)
        (cond 
          [(zero? (complexnum-imag-part num))
            (exact (complexnum-real-part num))]
          [(let ((i (complexnum-imag-part num))
                 (r (complexnum-real-part num)))
              (and (rational? r) 
                   (rational? i)
                   (make-rectangular (exact r) (exact i))))]
          [else            
            (assertion-violation 'exact "no exact equivalent" num)])]
      [(flonum? num)
        (if (or (flnan? num) (flinfinite? num))
            (assertion-violation 'exact "no exact equivalent" num)
            (exact (flonum->ratnum num)))]
      [(bignum? num)
        (if (fx<=? (fixnum-width) (bitwise-length num))
            num
            (bignum->fixnum num))]
      [(ratnum? num)
        (if (= (ratnum-denominator num) 1)
            (exact (ratnum-numerator num))
            num)]
      [(rectnum? num)
        (if (zero? (rectnum-imag-part num))
            (exact (rectnum-real-part num))
            num)]
      [(fixnum? num)
         num]
      [else
        (assertion-violation 'exact "not a number" num)])) 
        
  (define/contract (div0 x1:real x2:real)
    (let* ((d (div x1 x2))
           (m (- x1 (* d x2))))
      (cond 
        [(< m (magnitude (/ x2 2))) d]
        [(positive? x2) (+ d 1)]
        [else (- d 1)])))
  
  (define/contract (mod x1:real x2:real)
    (- x1 (* (div x1 x2) x2)))

  (define/contract (mod0 x1:real x2:real)
    (- x1 (* (div0 x1 x2) x2)))
    
  (define/contract (div-and-mod x1:real x2:real)
    (let ((d (div x1 x2)))
      (values d (- x1 (* d x2)))))             

  (define/contract (div0-and-mod0 x1:real x2:real)
    (let ((d (div0 x1 x2)))
      (values d (- x1 (* d x2)))))         
        
  (define (hex-char num)
    (integer->char ($fx+ num (char->integer (if ($fx<? num 10) #\0 #\W)))))
        
  (define (fixnum->string num radix)
    (if ($fxnegative? num)
        (string-append "-" (number->string (abs num) radix))
        (clr-static-call Convert (ToString Int32 Int32) num radix)))
     
  (define (bignum->string num radix)
    (let* ((neg? (negative? num))
           (num  (abs num))
           (out  (let f ((num num)(a '()))
                   (if (zero? num)
                       (apply string a)
                       (f (div num radix)
                          (cons (hex-char (mod num radix)) a))))))
       (if neg?
           (string-append "-" out)
           out)))
        
  (define number->string
    (case-lambda
      [(num)
        (number->string num 10)]
      [(num radix prec)
        (number->string num radix)]
      [(num radix)
        (cond
          [(fixnum? num)
            (fixnum->string num radix)]
          [(flonum? num)
            (unless (= radix 10)
              (assertion-violation 'number->string "invalid radix" radix))
            (flonum->string num)]
          [(ratnum? num)
            (string-append (if (negative? num) "-" "") 
                           (number->string (abs (ratnum-numerator num)) radix)
                           "/"
                           (number->string (abs (ratnum-denominator num)) radix))]
          [(bignum? num)
            (bignum->string num radix)]
          [(complexnum? num)
            (unless (= radix 10)
              (assertion-violation 'number->string "invalid radix" radix))
            (let ((rp (real-part num))
                  (ip (imag-part num)))
              (string-append (if (and (zero? rp) (not (and (flonum? rp) (clr-static-call IntX IsNegativeZero rp))))
                                 "" 
                                 (number->string rp radix))
                             (if (or (negative? ip)
                                     (nan? ip)
                                     (infinite? ip)
                                     (and (flonum? ip) (clr-static-call IntX IsNegativeZero ip)))
                                 "" 
                                 "+")
                             (number->string ip radix)
                             "i"))]
          [(rectnum? num)
            (string-append (if (zero? (real-part num)) 
                               "" 
                               (number->string (real-part num) radix))
                           (if (negative? (imag-part num)) "" "+")
                           (if (= 1 (imag-part num))
                               ""
                               (number->string (imag-part num) radix))
                           "i")]
          [else
            (assertion-violation 'number->string "not a number" num)])]))  
            

  (define-syntax exact-compare
    (syntax-rules ()
      [(_ x y)
        (clr-static-call Builtins ExactCompare x y)]))                       

  (define-syntax inexact-compare
    (syntax-rules ()
      [(_ x y)
        (clr-static-call Builtins InexactCompare x y)]))    

  (define-syntax define-comparer 
    (lambda (x)
      (syntax-case x ()
        [(_ name)
          (with-syntax ((uname 
              (datum->syntax #'name
                (string->symbol
                  (string-append 
                    "$fx"
                    (symbol->string (syntax->datum #'name))
                    "?")))))
            #'(define name
                (case-lambda
                  [(a) 
                    (if (number? a)
                        #t
                        (assertion-violation 'name "not a number" a))]
                  [(a b)
                    (cond 
                      [(and (real? a)
                            (real? b)
                            (finite? a)
                            (finite? b)
                            (not (nan? a))
                            (not (nan? b)))
                        (uname (exact-compare (exact a) (exact b)) 0)]
                      [(or (and (real? a) (nan? a)) 
                           (and (real? b) (nan? b))) #f]
                      [(and (number? a) (number? b))
                        (inexact=? (inexact a) (inexact b))]
                      [else
                        (assertion-violation 'name "not number arguments" a b)])]
                  [(x1 x2 . rest)
                    (let f ((a x1)(b (cons x2 rest)))
                      (cond 
                        [(null? b) #t]
                        [(name a ($car b))
                          (f ($car b) ($cdr b))]
                        [else #f]))])))])))
                        
  (define-syntax define-real-comparer 
    (lambda (x)
      (syntax-case x ()
        [(_ name)
          (with-syntax ((uname 
              (datum->syntax #'name
                (string->symbol
                  (string-append 
                    "$fx"
                    (symbol->string (syntax->datum #'name))
                    "?")))))
            #'(define name
                (case-lambda
                  [(a b)
                    (cond 
                      [(and (real? a) 
                            (real? b)
                            (finite? a)
                            (finite? b)
                            (not (nan? a))
                            (not (nan? b)))
                        (uname (exact-compare (exact a) (exact b)) 0)]
                      [(or (and (real? a) (nan? a)) 
                           (and (real? b) (nan? b))) #f]
                      [(and (real? a)
                            (real? b))
                        (uname (inexact-compare (inexact a) (inexact b)) 0)]                                                
                      [else
                        (assertion-violation 'name "not real arguments" a b)])]
                  [(x1 x2 . rest)
                    (let f ((a x1)(b (cons x2 rest)))
                      (cond 
                        [(null? b) #t]
                        [(name a ($car b))
                          (f ($car b) ($cdr b))]
                        [else #f]))])))])))                        
                        
  (define-comparer =)
  (define-real-comparer <)
  (define-real-comparer <=)
  (define-real-comparer >)
  (define-real-comparer >=)   
  
  (define/contract (make-rectangular r1:real r2:real)
    (cond 
      [(and (exact? r1) (exact? r2))
        (make-rectnum (->ratnum r1) (->ratnum r2))]
      [(and (exact? r2) (zero? r2)) r1]
      [else 
        (make-complexnum (inexact r1) (inexact r2))]))
      
  (define/contract (make-polar r1:real r2:real)
    (if (and (exact? r2) (zero? r2))
      r1      
      (* r1 (make-rectangular (cos r2) (sin r2)))))
        
  (define/contract (angle num:number)
    (if (rectnum? num)
        (angle (inexact num))      
        (atan (imag-part num)
              (real-part num))))
          
  (define (magnitude num)
    (cond
      [(rectnum? num)
        (magnitude (inexact num))]
      [(complexnum? num)
        (let ((i (imag-part num))
              (r (real-part num)))
          (sqrt (+ (* i i) (* r r))))]   
      [(real? num)
        (abs num)]              
      [else
        (assertion-violation 'magnitude "not a number" num)]))
    
  (define (exact-integer? obj)
    (or (fixnum? obj)
        (bignum? obj)))    
    
  (define (numerator num)
    (cond
      [(exact-integer? num) num]
      [(ratnum? num)
        (exact (ratnum-numerator num))]
      [(and (real? num)
            (finite? num)
            (not (nan? num)))
        (inexact (numerator (exact num)))]
      [else
        (assertion-violation 'numerator "not a real" num)]))

  (define (denominator num)
    (cond
      [(exact-integer? num) 1]
      [(ratnum? num)
        (exact (ratnum-denominator num))]
      [(and (real? num)
            (finite? num)
            (not (nan? num)))
        (inexact (denominator (exact num)))]
      [else
        (assertion-violation 'denominator "not a real" num)]))
        
  (define (real-part num)
    (cond
      [(complexnum? num)
        (complexnum-real-part num)]
      [(rectnum? num)
        (exact (rectnum-real-part num))]
      [(real? num) num]
      [else 
        (assertion-violation 'real-part "not a number" num)]))

  (define (imag-part num)
    (cond
      [(complexnum? num)
        (complexnum-imag-part num)]
      [(rectnum? num)
        (exact (rectnum-imag-part num))]
      [(real? num) 0]
      [else 
        (assertion-violation 'imag-part "not a number" num)]))
        
  (define-syntax define-math-proc
    (syntax-rules ()
      [(_ name clr-name)
        (define (name num)
          (cond
            [(rectnum? num)
              (name (rectnum->complexnum num))]
            [(complexnum? num)
              (clr-static-call Complex64 clr-name num)]
            [(real? num)
              (clr-static-call Math clr-name (inexact num))]
            [else
              (assertion-violation 'name "not a number" num)]))]))
              
  (define-syntax define-math-proc*
    (syntax-rules ()
      [(_ name clr-name)
        (define (name num)
          (cond
            [(rectnum? num)
              (name (rectnum->complexnum num))]
            [(complexnum? num)
              (clr-static-call Complex64 clr-name num)]
            [(real? num)
              (name (make-rectangular num 0.0))]
            [else
              (assertion-violation 'name "not a number" num)]))]))              
              
  (define-math-proc exp Exp)
  (define-math-proc sin Sin)
  (define-math-proc* asin Asin)
  (define-math-proc* sinh Sinh)
  (define-math-proc cos Cos)
  (define-math-proc* acos Acos)
  (define-math-proc* cosh Cosh)
  (define-math-proc tan Tan)
  (define-math-proc* tanh Tanh)
  
  (define atan
    (case-lambda
      [(num)
        (cond
          [(rectnum? num)
              (atan (rectnum->complexnum num))]
          [(complexnum? num)
            (clr-static-call Complex64 Atan num)]
          [(real? num)
            (clr-static-call Math Atan (inexact num))]
          [else
            (assertion-violation 'atan "not a number" num)])]
      [(num1 num2)
        (unless (real? num1)
          (assertion-violation 'atan "not a real" num1))
        (unless (real? num2)
          (assertion-violation 'atan "not a real" num2))
        (clr-static-call Math Atan2 (inexact num1) (inexact num2))]))
            
  (define log
    (case-lambda
      [(num)
        (unless (number? num)
          (assertion-violation 'atan "not a number" num))
        (cond
          [(rectnum? num)
              (log (rectnum->complexnum num))]
          [(complexnum? num) 
            (clr-static-call Complex64 Log num)]
          [(negative? num) 
            (clr-static-call Complex64 
                             Log 
                             (make-complexnum (inexact num) 0.0))]
          [(zero? num)
            (if (exact? num)
              (assertion-violation 'log "not possible" num)
              -inf.0)]
          [(infinite? num)
            (if (negative? num)
              (make-complexnum (inexact (abs num)) 0)
              num)]
          [else
            (clr-static-call Math Log (inexact num))])]
      [(num1 num2)
        (/ (log num1) (log num2))]))
        
        
  (define/contract (div x1:real x2:real)
    (when (zero? x2)
      (assertion-violation 'div "divide by zero" x1 x2))
    (when (or (nan? x1) (infinite? x1))
      (assertion-violation 'div "cannot be nan or infinite" x1 x2))
    (let-values (((x1 x2 exact-args?) 
                  (if (and (exact? x1) (exact? x2))
                      (let ((scale (* (denominator x1)
                                      (denominator x2))))
                        (values (* x1 scale)
                                (* x2 scale)
                                #t))
                      (values x1 x2 #f))))
       (let ((d (if (positive? x2)
                    (floor (/ x1 x2))
                    (- (floor (/ x1 (- x2)))))))
         (if (and exact-args? (rational-valued? d))
             (exact d)
             d))))
             
  (define/contract (abs x1:real)
    (if (negative? x1)
        (- x1)
        x1))
        
  (define/contract (floor x:real)
    (cond
      [(exact-integer? x) x]
      [(ratnum? x)
        (let ((r (bignum/ (ratnum-numerator x) (ratnum-denominator x))))
          (exact (if (negative? x) (- r 1) r)))]
      [else
        (clr-static-call Math (Floor Double) (inexact x))]))
             
  (define/contract (ceiling x:real)
    (cond
      [(exact-integer? x) x]
      [(ratnum? x)
        (let ((r (bignum/ (ratnum-numerator x) (ratnum-denominator x))))
          (exact (if (positive? x) (+ r 1) r)))]
      [else
        (clr-static-call Math (Ceiling Double) (inexact x))]))

  (define/contract (truncate x:real)
    (cond
      [(exact-integer? x) x]
      [else
        (let ((r (clr-static-call Math (Truncate Double) (inexact x))))
          (if (exact? x)
              (exact r)
              r))]))
            
  (define/contract (round x:real)
    (cond
      [(exact-integer? x) x]
      [(ratnum? x)
        (let* ((num (ratnum-numerator x))
               (den (ratnum-denominator x))
               (d (bignum/ num den))
               (r (bignum% num den))
               (hd (div d 2)))
          (cond
            [(negative? r)
              (exact (cond 
                       [(> (- r) hd) (- d 1)]
                       [(< (- r) hd) d]
                       [(even? d) d]
                       [else (+ d 1)]))]
            [(positive? r)
              (exact (cond 
                       [(> r hd) (+ d 1)]
                       [(< r hd) d]
                       [(even? d) d]
                       [else (+ d 1)]))]
            [else d]))]
      [else
        (clr-static-call Math (Round Double) (inexact x))]))
        
        
  (define/contract (sqrt num:number)
    (cond
      [(rectnum? num)
        (sqrt (rectnum->complexnum num))]
      [(complexnum? num)
        (clr-static-call Complex64 Sqrt num)]
      [(negative? num)
        (make-rectangular 0 (sqrt (- num)))]
      [(bignum? num)
        (bignum-sqrt num)]
      [(infinite? num) num]
      [else
        (let ((r (clr-static-call Math Sqrt (inexact num))))
          (if (exact? num)
              (exact r)
              r))]))
              
  (define/contract (even? n:integer)
    (zero? (mod n 2)))

  (define/contract (odd? n:integer)
    (not (zero? (mod n 2))))
  
  (define/contract (max a:real . rest:real)
    (fold-left 
      (lambda (a b) 
        (let ((r (if (< a b) b a)))
          (if (or (inexact? a) (inexact? b))
              (inexact r)
              r)))
      a 
      rest))
    
  (define/contract (min a:real . rest:real)
    (fold-left 
      (lambda (a b) 
        (let ((r (if (> a b) b a)))
          (if (or (inexact? a) (inexact? b))
              (inexact r)
              r)))
      a 
      rest))   
    
  (define/contract gcd
    (case-lambda
      [() 0]
      [(a:integer) (abs a)]
      [(a:integer b:integer) 
        (if (zero? b)
            (abs a)
            (abs (gcd b (mod a b))))]
      [(a:integer b:integer . rest)
        (fold-left gcd (abs a) (cons b rest))]))
          
  (define/contract lcm
    (case-lambda
      [() 1]
      [(a:integer) (abs a)]
      [(a:integer b:integer) 
        (if (or (zero? a)(zero? b))
            0
            (abs (* (/ a (gcd a b)) b)))]
      [(a:integer b:integer . rest)
        (fold-left lcm (abs a) (cons b rest))]))
              
  ;; from SLIB
  (define/contract (rationalize x:real e:real) 
    (if (and (infinite? x) (infinite? e))
        +nan.0
        (let ((r (apply / (find-ratio x e))))
          (if (and (exact? x) (exact? e))
              r
              (inexact r)))))

  (define (find-ratio x e) 
    (find-ratio-between (- x e) (+ x e)))

  (define (find-ratio-between x y)
    (define (sr x y)
      (let ((fx (exact (floor x))) 
            (fy (exact (floor y))))
        (cond 
          ((>= fx x) (list fx 1))
          ((= fx fy) (let ((rat (sr (/ (- y fy)) (/ (- x fx)))))
		                   (list (+ (cadr rat) (* fx (car rat))) (car rat))))
          (else (list (+ 1 fx) 1)))))
    (cond 
      ((< y x) (find-ratio-between y x))
      ((>= x y) (list x 1))
      ((positive? x) (sr x y))
      ((negative? y) (let ((rat (sr (- y) (- x))))
	                     (list (- (car rat)) (cadr rat))))
      (else '(0 1))))              
                
  (define (exact-integer-sqrt num)
    (if (bignum? num)
        (bignum-sqrt-exact num)
        (let* ((r (sqrt num))
               (rf (exact (floor r)))
               (rest (- num (* rf rf))))
          (values rf rest))))
  
          
  (define (expt obj1 obj2)
    (define (make-restriction-violation)
      (condition
        (make-implementation-restriction-violation)
        (make-who-condition 'expt)
        (make-message-condition "not supported")
        (make-irritants-condition obj1 obj2)))
    (cond
      [(rectnum? obj1)
        (expt (rectnum->complexnum obj1) obj2)]
      [(or (complexnum? obj1) (and (negative? obj1) 
                                   (not (and (real? obj1)
                                             (integer? obj2)))))
        (clr-static-call Complex64 
                         Pow 
                         (real->complexnum obj1)
                         (real->complexnum obj2))]
      [else
        (let ((e (and (exact? obj1) (exact? obj2)))
              (z1 (zero? obj1))
              (z2 (zero? obj2)))
          (cond
            [(and z1 (not z2))
              (if e 0 0.0)]
            [(or z2 (= obj1 1))
              (if e 1 1.0)]
            [(= obj2 1)
              (if e obj1 (inexact obj1))]
            [else
              (let* ((neg? (negative? obj2))
                     (obj2 (if neg? (abs obj2) obj2)))
                (cond
                  [(and e (integer? obj1) (integer? obj2))
                    (let* ((a (->bignum obj1))
                           (r (clr-call IntX
                                        Power
                                        a
                                        (->fixnum obj2))))
                       (if neg? 
                           (if (zero? r)
                               (raise (make-restriction-violation))
                               (/ 1 r))
                           (exact r)))]
                  [(and e (rational? obj1) (integer? obj2))
                    (let* ((f (->ratnum obj1)))
                       (if neg?
                           (/ (expt (denominator f) obj2) (expt (numerator f) obj2))
                           (/ (expt (numerator f) obj2) (expt (denominator f) obj2))))]
                  [(and (real? obj1) (real? obj2))
                    (let ((r (clr-static-call Math Pow (inexact obj1) (inexact obj2))))
                      (if neg? 
                          (/ 1 r)
                          r))]
                  [else 
                    (raise (make-restriction-violation))]))]))]))
                    
      
  (define/contract (fx*/carry fx1:fixnum fx2:fixnum fx3:fixnum)
    (let ((s (+ (* fx1 fx2) fx3))
          (e (expt 2 (fixnum-width))))
      (let-values (((d m) (div0-and-mod0 s e)))
        (values m d))))

  (define/contract (fx-/carry fx1:fixnum fx2:fixnum fx3:fixnum)
    (let ((s (- fx1 fx2 fx3))
          (e (expt 2 (fixnum-width))))
      (let-values (((d m) (div0-and-mod0 s e)))
        (values m d))))

  (define/contract (fx+/carry fx1:fixnum fx2:fixnum fx3:fixnum)
    (let ((s (+ fx1 fx2 fx3))
          (e (expt 2 (fixnum-width))))
      (let-values (((d m) (div0-and-mod0 s e)))
        (values m d))))
                  
;;; Free-format algorithm for printing IEEE double-precision positive
;;; floating-point numbers in base 10

;;; It uses the floating-point logarithm to estimate the scaling factor
;;; and a table to look up powers of ten.

;;; Input to flonum->digits:
;;;       v -- a positive floating-point number, f x 2^e
;;;       f -- mantissa of v
;;;       e -- exponent of v

;;; Output: (k d_1 d_2 ... d_n),
;;;   where 0.d_1...d_n x 10^k is the shortest correctly rounded base-10
;;;   number that rounds to v when input (it assumes the input
;;;   routine rounds to even)

;;; See also "Printing Floating-Point Numbers Quickly and Accurately"
;;; in Proceedings of the SIGPLAN '96 Conference on Programming Language
;;; Design and Implementation.

;;; Author: Bob Burger  Date: March 1996                            

  (define flonum->digits
    (lambda (v f e)
      (let ([min-e -1074]
            [bp-1 (expt 2 52)]
            [round? (even? f)])
        (if (>= e 0)
            (if (not (= f bp-1))
                (let ([be (expt 2 e)])
                  (scale (* f be 2) 2 be be 0 round? round? v))
                (let ([be (expt 2 e)])
                  (scale (* f be 4) 4 (* be 2) be 0 round? round? v)))
            (if (or (= e min-e) (not (= f bp-1)))
                (scale (* f 2) (expt 2 (- 1 e)) 1 1 0 round? round? v)
                (scale (* f 4) (expt 2 (- 2 e)) 2 1 0 round? round? v))))))

  (define scale
    (lambda (r s m+ m- k low-ok? high-ok? v)
      (let ([est (inexact->exact (ceiling (- (log10 v) 1e-10)))])
        (if (>= est 0)
            (fixup r (* s (expt10 est)) m+ m- est low-ok? high-ok?)
            (let ([scale (expt10 (- est))])
              (fixup (* r scale) s (* m+ scale) (* m- scale)
                     est low-ok? high-ok?))))))

  (define fixup
    (lambda (r s m+ m- k low-ok? high-ok?)
      (if ((if high-ok? >= >) (+ r m+) s) ; too low?
          (cons (+ k 1) (generate r s m+ m- low-ok? high-ok?))
          (cons k
                (generate (* r 10) s (* m+ 10) (* m- 10) low-ok? high-ok?)))))

  (define generate
    (lambda (r s m+ m- low-ok? high-ok?)
      (let ([d (quotient r s)]
            [r (remainder r s)])
        (let ([tc1 ((if low-ok? <= <) r m-)]
              [tc2 ((if high-ok? >= >) (+ r m+) s)])
          (if (not tc1)
              (if (not tc2)
                  (cons d (generate (* r 10) s (* m+ 10) (* m- 10)
                                    low-ok? high-ok?))
                  (list (+ d 1)))
              (if (not tc2)
                  (list d)
                  (if (< (* r 2) s)
                      (list d)
                      (list (+ d 1)))))))))

  (define expt10
    (let ([table (make-vector 326)])
      (do ([k 0 (+ k 1)] [v 1 (* v 10)])
          ((= k 326))
        (vector-set! table k v))
      (lambda (k)
        (vector-ref table k))))

  (define log10
    (let ([f (/ (log 10))])
      (lambda (x)
        (* (log x) f))))
        
; lets get printing!        

  (define (get-digits flo)
    (call-with-values (lambda () (decompose-flonum flo)) flonum->digits))  

  (define (get-chr i)
    (integer->char (+ (char->integer #\0)(fxmod i 10))))  
    
  (define (flonum->string flo)
    (cond
      [(flzero? flo) 
        (if (clr-static-call IntX IsNegativeZero flo)
            "-0.0"
            "0.0")]
      [(flnan? flo) "+nan.0"]
      [(flinfinite? flo) 
        (if (flpositive? flo)
            "+inf.0"
            "-inf.0")]
      [else
        (let-values (((p r) (open-string-output-port)))
          (let* ((d (get-digits (flabs flo)))
                 (k (car d))
                 (n (cdr d)))
            (when (flnegative? flo)
              (put-string p "-"))
            (cond
              [(<= 0 k 9)
                ; print small positive exponent
                (let f ((i 0)(n n))
                  (cond
                    [(null? n)
                      (if (< i k)
                          (begin
                            (put-string p "0")
                            (f (+ i 1) n))
                          (begin
                            (when (= i k)
                              (put-string p ".0"))
                            (r)))]
                    [else
                      (when (= i k)
                        (when (zero? k)
                          (put-string p "0"))
                        (put-string p "."))
                      (put-char p (get-chr (car n)))
                      (f (+ i 1) (cdr n))]))]
              [(<= -3 k 0)
                ; print small negative exponent
                (put-string p "0.")
                (let f ((i k))
                  (unless (zero? i)
                    (put-string p "0")
                    (f (+ i 1))))
                (let f ((n n))
                  (unless (null? n)
                    (put-char p (get-chr (car n)))
                    (f (cdr n))))
                (r)]
              [else
                ; print with exponent
                (put-char p (get-chr (car n)))
                (put-string p ".")
                (let f ((i 0)(n (cdr n)))
                  (unless (null? n)
                    (put-char p (get-chr (car n)))
                    (f (+ i 1) (cdr n))))
                (put-string p "e")
                (display (- k 1) p)
                (r)])))])))
  
  