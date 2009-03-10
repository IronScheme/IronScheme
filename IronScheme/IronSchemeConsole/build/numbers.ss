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
    number?
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
    round)
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
      number?
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
      round)
    (ironscheme unsafe)
    (ironscheme clr))

  (define (bignum? obj)
    (clr-is Microsoft.Scripting.Math.BigInteger obj))
  
  (define (ratnum? obj)
    (clr-is IronScheme.Runtime.Fraction obj))
    
  (define (ratnum-denominator rat)
    (clr-prop-get IronScheme.Runtime.Fraction Denominator rat))   
    
  (define (ratnum-numerator rat)
    (clr-prop-get IronScheme.Runtime.Fraction Numerator rat))  
    
  
  (define (complexnum? obj)
    (clr-is Microsoft.Scripting.Math.Complex64 obj))
    
  (define (make-complexnum r1 r2)
    (clr-static-call Microsoft.Scripting.Math.Complex64 Make r1 r2))
    
  (define (complexnum-imag-part c)
    (clr-prop-get Microsoft.Scripting.Math.Complex64 Imag c))
    
  (define (complexnum-real-part c)
    (clr-prop-get Microsoft.Scripting.Math.Complex64 Real c))
   
    
  (define (number? obj)
    (or (fixnum? obj)
        (flonum? obj)
        (bignum? obj)
        (ratnum? obj)
        (complexnum? obj)))
        
  (define (nan? num)
    (cond
      [(or (fixnum? num)
           (bignum? num)
           (ratnum? num))
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
           (ratnum? num))
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
           (ratnum? num))
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
           (ratnum? obj))
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
           (ratnum? obj))
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
      [(complexnum? obj)
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
      [(complexnum? obj)
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
                (flonum? obj))
            (finite? obj) 
            (not (nan? obj)))            
        (let ((i (imag-part obj)))
          (and (zero? i)
               (= (denominator (real-part obj)) 1)))]
      [else #f]))                

  (define (zero? num)
    (= num 0))
    
  (define (positive? num)
    (> num 0))
      
  (define (negative? num)
    (< num 0))

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
                  [(a b)
                    (cond 
                      [(and (real? a) 
                            (real? b)
                            (finite? a)
                            (finite? b)
                            (not (nan? a))
                            (not (nan? b)))
                        (uname (exact-compare (exact a) (exact b)) 0)]
                      [(or (nan? a) (nan? b)) #f]
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
                      [(or (nan? a) (nan? b)) #f]
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
  
  (define (make-rectangular r1 r2)
    (unless (real? r1)
      (assertion-violation 'make-rectangular "not a real" r1))
    (unless (real? r2)
      (assertion-violation 'make-rectangular "not a real" r2))
    (if (and (exact? r2) (zero? r2))
      r1
      (make-complexnum (inexact r1) (inexact r2))))
      
  (define (make-polar r1 r2)
    (unless (real? r1)
      (assertion-violation 'make-polar "not a real" r1))
    (unless (real? r2)
      (assertion-violation 'make-polar "not a real" r2))
    (if (and (exact? r2) (zero? r2))
      r1      
      (* r1 (make-rectangular (cos r2) (sin r2)))))
        
  (define (angle num)
    (unless (number? num)
      (assertion-violation 'angle "not a number" num))
    (atan (imag-part num)
          (real-part num)))
          
  (define (magnitude num)
    (cond
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
      [(real? num) num]
      [else 
        (assertion-violation 'real-part "not a number" num)]))

  (define (imag-part num)
    (cond
      [(complexnum? num)
        (complexnum-imag-part num)]
      [(real? num) 0]
      [else 
        (assertion-violation 'imag-part "not a number" num)]))
        
  (define-syntax define-math-proc
    (syntax-rules ()
      [(_ name)
        (define (name num)
          (cond
            [(complexnum? num)
              (clr-static-call Microsoft.Scripting.Math.Complex64 name num)]
            [(real? num)
              (clr-static-call System.Math name (inexact num))]
            [else
              (assertion-violation 'name "not a number" num)]))]))
              
  (define-math-proc exp)
  (define-math-proc sin)
  (define-math-proc asin)
  (define-math-proc sinh)
  (define-math-proc cos)
  (define-math-proc acos)
  (define-math-proc cosh)
  (define-math-proc tan)
  (define-math-proc tanh)
  
  (define atan
    (case-lambda
      [(num)
        (cond
          [(complexnum? num)
            (clr-static-call Microsoft.Scripting.Math.Complex64 Atan num)]
          [(real? num)
            (clr-static-call System.Math Atan (inexact num))]
          [else
            (assertion-violation 'atan "not a number" num)])]
      [(num1 num2)
        (unless (real? num1)
          (assertion-violation 'atan "not a real" num1))
        (unless (real? num2)
          (assertion-violation 'atan "not a real" num2))
        (clr-static-call System.Math Atan2 (inexact num1) (inexact num2))]))
            
  (define log
    (case-lambda
      [(num)
        (unless (number? num)
          (assertion-violation 'atan "not a number" num))
        (cond
          [(complexnum? num) 
            (clr-static-call Microsoft.Scripting.Math.Complex64 Log num)]
          [(negative? num) 
            (clr-static-call Microsoft.Scripting.Math.Complex64 
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
            (clr-static-call System.Math Log (inexact num))])]
      [(num1 num2)
        (/ (log num1) (log num2))]))
        
        
  (define (div x1 x2)
    (unless (real? x1)
      (assertion-violation 'div "not a real" x1))
    (unless (real? x2)
      (assertion-violation 'div "not a real" x2))
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
             
  (define (abs x1)
    (unless (real? x1)
      (assertion-violation 'abs "not a real" x1))
    (if (negative? x1)
        (- x1)
        x1))
        
  (define (bignum/ a b)
    (clr-static-call Microsoft.Scripting.Math.BigInteger op_Division a b))    
    
  (define (bignum% a b)
    (clr-static-call Microsoft.Scripting.Math.BigInteger op_Modulus a b))
    
  (define (bignum->fixnum b)
    (clr-call Microsoft.Scripting.Math.BigInteger ToInt32 b))
    
  (define (flonum->ratnum f)
    (clr-static-call IronScheme.Runtime.Fraction "op_Implicit(System.Double)" f))

  (define (ratnum->flonum r)
    (clr-call IronScheme.Runtime.Fraction ToDouble r '()))
    
  (define (fixnum->bignum f)
    (clr-static-call Microsoft.Scripting.Math.BigInteger "Create(System.Int32)" f))
        
  (define (floor x)
    (unless (real? x)
      (assertion-violation 'floor "not a real" x))
    (cond
      [(exact-integer? x) x]
      [(ratnum? x)
        (let ((r (bignum/ (ratnum-numerator x) (ratnum-denominator x))))
          (exact (if (negative? x) (- r 1) r)))]
      [else
        (clr-static-call System.Math "Floor(System.Double)" (inexact x))]))
             
  (define (ceiling x)
    (unless (real? x)
      (assertion-violation 'ceiling "not a real" x))
    (cond
      [(exact-integer? x) x]
      [(ratnum? x)
        (let ((r (bignum/ (ratnum-numerator x) (ratnum-denominator x))))
          (exact (if (positive? r) (+ r 1) r)))]
      [else
        (clr-static-call System.Math "Ceiling(System.Double)" (inexact x))]))

  (define (truncate x)
    (unless (real? x)
      (assertion-violation 'truncate "not a real" x))
    (cond
      [(exact-integer? x) x]
      [else
        (let ((r (clr-static-call System.Math "Truncate(System.Double)" (inexact x))))
          (if (exact? x)
              (exact r)
              r))]))
            
  (define (round x)
    (unless (real? x)
      (assertion-violation 'round "not a real" x))
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
        (clr-static-call System.Math "Round(System.Double)" (inexact x))]))
)
  
  