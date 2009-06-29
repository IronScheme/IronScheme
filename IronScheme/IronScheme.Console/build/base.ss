#| ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 2007,2008,2009
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************|#

(library (ironscheme base)
  (export
    define
    define-syntax
    quote
    lambda
    if
    set!
    cond
    ...
    =>
    else
    case
    and
    or
    let
    let*
    letrec
    letrec*
    let-values
    let*-values
    begin
    
    eqv?
    eq?
    equal?
    
    real-valued?
    rational-valued?
    integer-valued?
    
    inexact
    exact
    
    odd?
    even?
    finite?
    infinite?
    nan?
    
    max
    min
    
    +
    *
    -
    /
    
    abs
    div-and-mod
    div
    mod
    div0-and-mod0
    div0
    mod0
    
    gcd
    lcm
    
    numerator
    denominator
    
    floor
    ceiling
    truncate
    round
    
    rationalize
    
    exp
    log
    sin
    cos
    tan
    asin
    acos
    atan
    
    sqrt
    exact-integer-sqrt
    expt
    
    make-rectangular
    make-polar
    real-part
    imag-part
    magnitude
    angle
    
    number->string
    string->number
    
    not
    boolean?
    boolean=?
    
    pair?
    cons
    car
    cdr
    caar
    cadr
    cdar
    cddr
    caaar
    caadr
    cadar
    caddr
    cdaar
    cdadr
    cddar
    cdddr
    caaaar
    caaadr
    caadar
    caaddr
    cadaar
    cadadr
    caddar
    cadddr
    cdaaar
    cdaadr
    cdadar
    cdaddr
    cddaar
    cddadr
    cdddar
    cddddr
    null?
    list?
    list
    length
    append
    reverse
    list-tail
    list-ref
    map
    for-each
    
    symbol?
    symbol->string
    symbol=?
    string->symbol
    
    char?
    char->integer
    integer->char
    char=?
    char<?
    char>?
    char<=?
    char>=?
    
    string?
    make-string
    string
    string-length
    string-ref
    string=?
    string<?
    string>?
    string<=?
    string>=?
    substring
    string-append
    string->list
    list->string
    string-for-each
    string-copy
    
    vector?
    make-vector
    vector
    vector-length
    vector-ref
    vector-set!
    vector->list
    list->vector
    vector-fill!
    vector-map
    vector-for-each
    
    error
    assertion-violation
    assert
    
    apply
    call-with-current-continuation
    call/cc
    values
    call-with-values
    dynamic-wind
    procedure?
    
    quasiquote
    unquote
    unquote-splicing
    
    let-syntax
    letrec-syntax
    syntax-rules
    identifier-syntax

    fixnum?
    flonum?
    fixnum-width
    bytevector?
    
    string-set!
    string-fill!
  
    )
  (import 
    (except (ironscheme) 
      caar
      cadr
      cdar
      cddr
      caaar
      caadr
      cadar
      caddr
      cdaar
      cdadr
      cddar
      cdddr
      caaaar
      caaadr
      caadar
      caaddr
      cadaar
      cadadr
      caddar
      cadddr
      cdaaar
      cdaadr
      cdadar
      cdaddr
      cddaar
      cddadr
      cdddar
      cddddr
      char=?
      char<?
      char>?
      char<=?
      char>=?
      string=?
      string<?
      string>?
      string<=?
      string>=?
      symbol=?
      boolean=?     
      rationalize
      max
      min
      even?
      odd?
      gcd
      lcm
      vector-map
      vector-for-each
      string-for-each
      reverse
      vector-fill!
      char?
      vector?
      bytevector?
      symbol?
      boolean?      
      procedure?

      flonum?

      ;fixnum?
      ;fixnum-width
      ;
      mod
      mod0   
      div-and-mod
      div0-and-mod0
      div0
      
      list->vector
      list->string 
      
      vector-ref
      vector-set!  

      symbol->string
      string->symbol 
      
      make-vector
      vector-length  
      
      string? 
      make-string  
      string-ref
      string-set!
      string-fill!
      string-length
      string-copy
      substring
      string-append
      string->list
      string
      
      string-format
      string-compare
      
      vector->list
      char->integer
      integer->char
      
      ;current-input-port
      ;current-output-port
      ;current-error-port
      
     )
    (ironscheme clr)
    (ironscheme unsafe))
    
    (define (clr-string? obj)
      (clr-is System.String obj))

    (define (stringbuilder? obj)
      (clr-is System.Text.StringBuilder obj))
      
    (define (string? obj)
      (or (clr-string? obj) 
          (stringbuilder? obj)))
  
    (define (char? obj)
      (clr-is System.Char obj))
      
    (define (vector? obj)
      (clr-is System.Object[] obj))

    (define (bytevector? obj)
      (clr-is System.Byte[] obj))

    (define (symbol? obj)
      (clr-is Microsoft.Scripting.SymbolId obj))
      
    (define (boolean? obj)
      (clr-is System.Boolean obj))
     
    (define (procedure? obj)
      (clr-is Ironscheme.Runtime.Callable obj))  
      

    (define (flonum? obj)
      (clr-is System.Double obj))  
            
    
    (define (char->integer chr)
      (unless (char? chr)
        (assertion-violation 'char->integer "not a character" chr))
      (clr-cast System.Int32 (clr-cast System.Char chr)))
      
    (define (integer->char num)
      (unless (fixnum? num)
        (assertion-violation 'integer->char "not a integer" num))
      (when (or (fxnegative? num)      
                (fx>? num #x10ffff)
                (and (fx>? num #xd7ff)
                     (fx<? num #xe000)))
        (assertion-violation 'integer->char "not a valid unicode value" num))
      (string-ref (clr-static-call System.Char ConvertFromUtf32 num) 0))
      
    (define make-string
      (case-lambda
        [(k)
          (make-string k #\nul)]
        [(k fill)
          (unless (fixnum? k)
            (assertion-violation 'make-string "not a fixnum" k))
          (when (negative? k)
            (assertion-violation 'make-string "cannot be negative" k))        
          (unless (char? fill)
            (assertion-violation 'make-string "not a character" fill))
          (let ((str (clr-new System.String (clr-cast System.Char fill) (clr-cast System.Int32 k))))            
            (clr-new System.Text.StringBuilder (clr-cast System.String str)))]))

    (define (string-ref str k)
      (unless (and (fixnum? k) (fx>=? k 0))
        (assertion-violation 'string-ref "not a non-negative integer" k))        
      (cond
        [(clr-string? str) 
          (clr-prop-get System.String Chars str k)]
        [(stringbuilder? str) 
          (clr-prop-get System.Text.StringBuilder Chars str k)]
        [else
          (assertion-violation 'string-set! "not a string" str)]))
            
    (define (string-set! str k val)
      (unless (stringbuilder? str)
        (assertion-violation 'string-set! "not a mutable string" str))
      (unless (and (fixnum? k) (fx>=? k 0))
        (assertion-violation 'string-set! "not a non-negative integer" k))        
      (clr-prop-set! System.Text.StringBuilder Chars str k val))
      
    (define (string-fill! str k fill)
      (unless (stringbuilder? str)
        (assertion-violation 'string-fill! "not a mutable string" str))
      (unless (and (fixnum? k) (fx>=? k 0))
        (assertion-violation 'string-fill! "not a non-negative integer" k))        
      (unless (char? fill)
        (assertion-violation 'string-fill! "not a character" fill))
      (let f ((i 0))
        (unless (fx=? i k)
          (clr-prop-set! System.Text.StringBuilder Chars str i fill)
          (f (fx+ i 1)))))
            
    (define (string-length str)
      (cond
        [(clr-string? str) 
          (clr-prop-get System.String Length str)]
        [(stringbuilder? str) 
          (clr-prop-get System.Text.StringBuilder Length str)]
        [else
          (assertion-violation 'string-length "not a string" str)]))
          
    (define (->string str)
      (if (clr-string? str)
          str
          (clr-call Object ToString str)))
          
    (define (string . args)
      (unless (for-all char? args)
        (assertion-violation 'string "not all char" args))
      (let ((str (clr-new System.Text.StringBuilder)))
        (let f ((args args))
          (if (null? args)
              str
              (begin
                (clr-call System.Text.StringBuilder "Append(Char)" str (car args))
                (f (cdr args)))))))
          
    (define (string->list str)
      (unless (string? str)
        (assertion-violation 'string->list "not a string" str))
      (clr-static-call IronScheme.Runtime.Cons FromList (->string str)))
          
    (define (string-copy str)
      (cond
        [(clr-string? str)
          (clr-static-call System.String Copy str)]
        [(stringbuilder? str)
          (clr-call System.Text.StringBuilder ToString str)]
        [else
          (assertion-violation 'string-copy "not a string" str)]))

    (define (substring str start end)
      (unless (and (fixnum? start) (fx>=? start 0))
        (assertion-violation 'substring "not a non-negative integer" start)) 
      (unless (and (fixnum? end) (fx>=? end 0))
        (assertion-violation 'substring "not a non-negative integer" end))             
      (cond
        [(clr-string? str)
          (clr-call System.String Substring str start (fx- end start))]
        [(stringbuilder? str)
          (clr-call System.Text.StringBuilder ToString str start (fx- end start))]
        [else
          (assertion-violation 'substring "not a string" str)]))
      
    ; probably need to be made faster  
    (define (string-append . args)
      (unless (for-all string? args)
        (assertion-violation 'string-append "not strings" args))
      (clr-static-call System.String 
                       "Concat(String[])"
                       (list->vector (map ->string args))))
                       
    (define (string-format fmt . args)
      (clr-static-call System.String "Format(String,Object[])" fmt (list->vector args)))                       
    
    (define (symbol->string sym)
      (unless (symbol? sym)
        (assertion-violation 'symbol->string "not a symbol" sym))
      (clr-static-call Microsoft.Scripting.SymbolTable IdToString sym))
      
    (define (string->symbol str)
      (unless (string? str)
        (assertion-violation 'string->symbol "not a string" str))
      (clr-static-call Microsoft.Scripting.SymbolTable StringToObject str))
      
    
    (define (div0 x1 x2)
      (let* ((d (div x1 x2))
             (m (- x1 (* d x2))))
        (cond 
          [(< m (magnitude (/ x2 2))) d]
          [(positive? x2) (+ d 1)]
          [else (- d 1)])))
    
    (define (mod x1 x2)
      (- x1 (* (div x1 x2) x2)))

    (define (mod0 x1 x2)
      (- x1 (* (div0 x1 x2) x2)))
      
    (define (div-and-mod x1 x2)
      (let ((d (div x1 x2)))
        (values d (- x1 (* d x2)))))             

    (define (div0-and-mod0 x1 x2)
      (let ((d (div0 x1 x2)))
        (values d (- x1 (* d x2)))))             
        
    (define (list->vector lst)
      (apply vector lst))        
      
    (define (list->string lst)
      (apply string lst))
      
    (define (vector-ref x n)
      (unless (vector? x)
        (assertion-violation 'vector-ref "not a vector" x))
      (unless (integer? n)
        (assertion-violation 'vector-ref "not an integer" n))
      (when (negative? n)
        (assertion-violation 'vector-ref "negative index" n))
      ($vector-ref x n))
      
    (define (vector-set! x n value)
      (unless (vector? x)
        (assertion-violation 'vector-set! "not a vector" x))
      (unless (integer? n)
        (assertion-violation 'vector-set! "not an integer" n))
      (when (negative? n)
        (assertion-violation 'vector-set! "negative index" n))
      ($vector-set! x n value)
      (void))   
      
    (define make-vector         
      (case-lambda
        [(k)
          (unless (fixnum? k)
            (assertion-violation 'make-vector "not a fixnum" k))
          (when (negative? k)
            (assertion-violation 'make-vector "cannot be negative" k))
          (clr-new-array System.Object k)]
        [(k fill)
          (let ((vec (make-vector k)))
            (vector-fill! vec fill)
            vec)]))
            
    (define (vector-length vec)
      (unless (vector? vec)
        (assertion-violation 'vector-length "not a vector" vec))
      (clr-prop-get System.Array Length vec))            
    
    (define (vector-fill! vec val)
      (let ((len (vector-length vec)))
        (do ((i 0 (fx+ i 1)))
            ((fx=? i len))
          (vector-set! vec i val))))  
          
    (define (vector->list vec)
      (unless (vector? vec)
        (assertion-violation 'vector->list "not a vector" vec))
      (clr-static-call IronScheme.Runtime.Cons FromList vec))   
      
    (define (reverse-helper l a)
      (if (null? l)
          a
          (reverse-helper (cdr l) (cons (car l) a))))
          
    (define (reverse lst)
      (reverse-helper lst '()))
     
    (define (caar   x) (car (car x)))
    (define (cadr   x) (car (cdr x)))
    (define (cdar   x) (cdr (car x)))
    (define (cddr   x) (cdr (cdr x)))

    (define (caaar  x) (caar (car x)))
    (define (caadr  x) (caar (cdr x)))
    (define (cadar  x) (cadr (car x)))
    (define (caddr  x) (cadr (cdr x)))
    (define (cdaar  x) (cdar (car x)))
    (define (cdadr  x) (cdar (cdr x)))
    (define (cddar  x) (cddr (car x)))
    (define (cdddr  x) (cddr (cdr x)))

    (define (caaaar x) (caaar (car x)))
    (define (caaadr x) (caaar (cdr x)))
    (define (caadar x) (caadr (car x)))
    (define (caaddr x) (caadr (cdr x)))
    (define (cadaar x) (cadar (car x)))
    (define (cadadr x) (cadar (cdr x)))
    (define (caddar x) (caddr (car x)))
    (define (cadddr x) (caddr (cdr x)))
    (define (cdaaar x) (cdaar (car x)))
    (define (cdaadr x) (cdaar (cdr x)))
    (define (cdadar x) (cdadr (car x)))
    (define (cdaddr x) (cdadr (cdr x)))
    (define (cddaar x) (cddar (car x)))
    (define (cddadr x) (cddar (cdr x)))
    (define (cdddar x) (cdddr (car x)))
    (define (cddddr x) (cdddr (cdr x)))
    
    ;(define current-input-port
      ;(make-parameter (clr-static-prop-get System.Console In)))
    ;
    ;(define current-output-port
      ;(make-parameter (clr-static-prop-get System.Console Out)))
;
    ;(define current-error-port
      ;(make-parameter (clr-static-prop-get System.Console Error)))
      
    (define (even? n)
      (unless (integer? n)
        (assertion-violation 'even? "not a integer" n))
      (= 0 (mod n 2)))           

    (define (odd? n)
      (unless (integer? n)
        (assertion-violation 'odd? "not a integer" n))
      (= 1 (mod n 2)))      
    
    (define (max a . rest)
      (unless (real? a)
        (assertion-violation 'max "not a real" a))    
      (fold-left 
        (lambda (a b) 
          (let ((r (if (< a b) b a)))
            (if (or (inexact? a) (inexact? b))
              (inexact r)
              r)))
        a 
        rest))
      
    (define (min a . rest)
      (unless (real? a)
        (assertion-violation 'min "not a real" a))    
      (fold-left 
        (lambda (a b) 
          (let ((r (if (> a b) b a)))
            (if (or (inexact? a) (inexact? b))
              (inexact r)
              r)))
        a 
        rest))   
      
    (define (gcd . nums)
      (case (length nums)
        [(0) 0]
        [(1)
          (let ((n (car nums)))
            (unless (integer? n)
              (assertion-violation 'gcd "not an integer" n))
            (abs n))]
        [(2)
          (let ((a (car nums))(b (cadr nums)))
            (unless (integer? a)
              (assertion-violation 'gcd "not an integer" a))
            (unless (integer? b)
              (assertion-violation 'gcd "not an integer" b))
            (if (zero? b)
              (abs a)
              (abs (gcd b (mod a b)))))]
        [else
          (fold-left gcd (abs (car nums)) (cdr nums))]))              
            
    (define (lcm . nums)
      (case (length nums)
        [(0) 1]
        [(1)
          (let ((n (car nums)))
            (unless (integer? n)
              (assertion-violation 'lcm "not an integer" n))
            (abs n))]
        [(2)
          (let ((a (car nums))(b (cadr nums)))
            (unless (integer? a)
              (assertion-violation 'lcm "not an integer" a))
            (unless (integer? b)
              (assertion-violation 'lcm "not an integer" b))
            (if (or (zero? a)(zero? b))
              0
              (abs (* (/ a (gcd a b)) b))))]
        [else
          (fold-left lcm (abs (car nums)) (cdr nums))])) 
          
    (define (string-compare a b)
      (clr-static-call System.String 
                       "Compare(String,String,StringComparison)"
                       (->string a) 
                       (->string b) 
                       'ordinal))
          
    (define-syntax define-string-compare
      (syntax-rules ()
        [(_ name cmp)
          (define name
            (lambda (a b . rest)
              (unless (string? a) (assertion-violation 'name "not a string" a))
              (for-all
                (lambda (x)
                  (unless (string? x) (assertion-violation 'name "not a string" x))  
                  (let ((r (cmp (clr-static-call System.String 
                                                 "Compare(String,String,StringComparison)"
                                                 (->string a)
                                                 (->string x) 
                                                 'ordinal) 0)))
                    (set! a x)
                    r))
                (cons b rest))))]))

    (define-string-compare string=? fx=?)
    (define-string-compare string<? fx<?)
    (define-string-compare string>? fx>?)
    (define-string-compare string<=? fx<=?)
    (define-string-compare string>=? fx>=?)

    (define (symbol=? a b . rest)
      (unless (symbol? a) (assertion-violation 'symbol=? "not a symbol" a))
      (for-all 
        (lambda (x) 
          (unless (symbol? x) (assertion-violation 'symbol=? "not a symbol" x))
          (eq? a x)) 
        (cons b rest)))

    (define (boolean=? a b . rest)
      (unless (boolean? a) (assertion-violation 'boolean=? "not a boolean" a))
      (for-all 
        (lambda (x) 
          (unless (boolean? x) (assertion-violation 'boolean=? "not a boolean" x))
          (eq? a x)) 
        (cons b rest)))
        
    (define-syntax define-char-compare
      (syntax-rules ()
        [(_ name cmp)
          (define name
            (lambda (a b . rest)
              (unless (char? a) (assertion-violation 'name "not a char" a))
              (for-all
                (lambda (x)
                  (unless (char? x) (assertion-violation 'name "not a char" x))  
                  (let ((r (cmp (char->integer a) (char->integer x))))
                    (set! a x)
                    r))
                (cons b rest))))]))        
    
    (define-char-compare char=? fx=?)
    (define-char-compare char<? fx<?)
    (define-char-compare char>? fx>?)
    (define-char-compare char<=? fx<=?)
    (define-char-compare char>=? fx>=?)
    
    ;; from SLIB
    (define (rationalize x e) 
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
        
    (define (vector-map p vec1 . vecs)
      (let* ((len (vector-length vec1))
             (res (make-vector len '())))
        (do ((i 0 (fx+ i 1)))
            ((fx=? i len) res)
          (vector-set! res i
            (if (null? vecs)
                (p (vector-ref vec1 i))
                (apply p (map (lambda (x) (vector-ref x i)) 
                              (cons vec1 vecs))))))))
          
    (define (vector-for-each p vec1 . vecs)
      (let ((len (vector-length vec1)))
        (do ((i 0 (fx+ i 1)))
            ((fx=? i len))
          (if (null? vecs)
              (p (vector-ref vec1 i))
              (apply p (map (lambda (x) (vector-ref x i)) 
                            (cons vec1 vecs)))))))
            
    (define (string-for-each p str1 . strs)
      (let ((len (string-length str1)))
        (do ((i 0 (fx+ i 1)))
            ((fx=? i len))
          (if (null? strs)
              (p (string-ref str1 i))
              (apply p (map (lambda (x) (string-ref x i)) 
                            (cons str1 strs)))))))

)
