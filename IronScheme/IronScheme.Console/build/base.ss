#| License
Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

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

    
    string?
    make-string
    string
    string-length
    string-ref

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
    
    string-format
  
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
 
      symbol=?
      boolean=?     
      vector-map
      vector-for-each
      string-for-each
      reverse
      vector-fill!

      list->vector
      list->string 
      
      vector-ref
      vector-set!  

      symbol->string
      string->symbol 
      
      make-vector
      vector-length  
      
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
      
      vector->list
      char->integer
      integer->char
     
     )
    (ironscheme contracts)
    (ironscheme clr)
    (ironscheme typed)
    (ironscheme core)
    (ironscheme unsafe))
    
    (clr-using System.Text)
    (clr-using IronScheme.Runtime)
    (clr-using Microsoft.Scripting)
    
    (define (char->integer chr)
      (unless (char? chr)
        (assertion-violation 'char->integer "not a char" chr))
      (clr-cast Int32 (clr-cast Char chr)))
      
    (define/contract (integer->char num:fixnum)
      (let: ((num : Int32 num))
        (when ($or? ($fxnegative? num)      
                    ($fx>? num #x10ffff)
                    ($and? ($fx>? num #xd7ff)
                           ($fx<? num #xe000)))
          (assertion-violation 'integer->char "not a valid unicode value" num))
        (string-ref (clr-static-call Char ConvertFromUtf32 num) 0)))
      
    (define/contract make-string
      (case-lambda
        [(k)
          (make-string k #\nul)]
        [(k:fixnum fill:char)
          (when ($fxnegative? k)
            (assertion-violation 'make-string "cannot be negative" k))        
          (let ((str (clr-new String (clr-cast Char fill) (clr-cast Int32 k))))            
            (clr-new StringBuilder (clr-cast String str)))]))

    (define (string-ref str k)
      (unless (and (fixnum? k) ($fx>=? k 0))
        (assertion-violation 'string-ref "not a non-negative integer" k))        
      (cond
        [(clr-string? str) 
          (clr-prop-get String Chars str k)]
        [(stringbuilder? str) 
          (clr-prop-get StringBuilder Chars str k)]
        [else
          (assertion-violation 'string-ref "not a string" str)]))
            
    (define (string-set! str k val)
      (unless (stringbuilder? str)
        (assertion-violation 'string-set! "not a mutable string" str))
      (unless (and (fixnum? k) ($fx>=? k 0))
        (assertion-violation 'string-set! "not a non-negative integer" k))
      (unless (char? val)
        (assertion-violation 'string-fill! "not a character" val))
      (clr-prop-set! StringBuilder Chars str k val))
      
    (define (string-fill! str k fill)
      (unless (stringbuilder? str)
        (assertion-violation 'string-fill! "not a mutable string" str))
      (unless (and (fixnum? k) ($fx>=? k 0))
        (assertion-violation 'string-fill! "not a non-negative integer" k))        
      (unless (char? fill)
        (assertion-violation 'string-fill! "not a character" fill))
      (let f ((i 0))
        (unless ($fx=? i k)
          (clr-prop-set! StringBuilder Chars str i fill)
          (f ($fx+ i 1)))))
            
    (define (string-length str)
      (cond
        [(clr-string? str) 
          (clr-prop-get String Length str)]
        [(stringbuilder? str) 
          (clr-prop-get StringBuilder Length str)]
        [else
          (assertion-violation 'string-length "not a string" str)]))
          
    (define ->string
      (typed-lambda (str) ((Object) String)
        (if (clr-string? str)
            str
            (clr-call Object ToString str))))
          
    (define/contract (string . args:char)
      (let ((str (clr-new StringBuilder)))
        (let f ((args args))
          (if (null? args)
              str
              (begin
                (clr-call StringBuilder (Append Char) str (car args))
                (f (cdr args)))))))
          
    (define/contract (string->list str:string)
      (clr-static-call Cons FromList (->string str)))
    
    (: ->mutable-string (String -> StringBuilder))
    
    (define: (->mutable-string str)
      (clr-new StringBuilder str)) 
          
    (define (string-copy str)
      (cond
        [(clr-string? str)
          (clr-static-call String Copy str)]
        [(stringbuilder? str)
          (->mutable-string (clr-call StringBuilder ToString str))]
        [else
          (assertion-violation 'string-copy "not a string" str)]))

    (define (substring str start end)
      (unless (and (fixnum? start) ($fx>=? start 0))
        (assertion-violation 'substring "not a non-negative integer" start)) 
      (unless (and (fixnum? end) ($fx>=? end 0))
        (assertion-violation 'substring "not a non-negative integer" end))             
      (cond
        [(clr-string? str)
          (clr-call String Substring str start (fx- end start))]
        [(stringbuilder? str)
          (->mutable-string (clr-call StringBuilder ToString str start (fx- end start)))]
        [else
          (assertion-violation 'substring "not a string" str)]))
      
    ; probably need to be made faster  
    (define/contract (string-append . args:string)
      (clr-static-call String 
                       (Concat String[])
                       (list->vector (map ->string args))))
                       
    (define/contract (string-format fmt:string . args)
      (clr-static-call String (Format String Object[]) fmt (list->vector args)))                       
    
    (define/contract (symbol->string sym:symbol)
      (clr-static-call SymbolTable IdToString sym))
      
    (define/contract (string->symbol str:string)
      (clr-static-call SymbolTable StringToObject (->string str)))
  
    (define/contract (list->vector lst:list)
      (apply vector lst))        
      
    (define/contract (list->string lst:list)
      (apply string lst))
      
    (define/contract (vector-ref x:vector n:fixnum)
      (when ($fxnegative? n)
        (assertion-violation 'vector-ref "negative index" n))
      (when ($fx>=? n (vector-length x))
        (assertion-violation 'vector-ref "index out of bounds" n))
      ($vector-ref x n))
      
    (define/contract (vector-set! x:vector n:fixnum value)
      (when ($fxnegative? n)
        (assertion-violation 'vector-set! "negative index" n))
      (when ($fx>=? n (vector-length x))
        (assertion-violation 'vector-set! "index out of bounds" n))
      ($vector-set! x n value)
      (void))   
      
    (define/contract make-vector         
      (case-lambda
        [(k:fixnum)
          (when ($fxnegative? k)
            (assertion-violation 'make-vector "cannot be negative" k))
          (clr-new-array Object k)]
        [(k fill)
          (let ((vec (make-vector k)))
            (vector-fill! vec fill)
            vec)]))
            
    (define/contract (vector-length vec:vector)
      (clr-prop-get Array Length vec))            
    
    (define/contract (vector-fill! vec:vector val)
      (let: ((vec : Object[] vec)
             (len : Int32 (vector-length vec)))
        (do ((i 0 ($fx+ i 1)))
            (($fx=? i len))
          ($vector-set! vec i val))))
          
    (define/contract (vector->list vec:vector)
      (clr-static-call Cons FromList vec))   
      
    (define/contract (reverse lst:list)
      (let f ((l lst)(a '()))
        (if (null? l)
            a
            (f ($cdr l) (cons ($car l) a)))))
     
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
        
    (define/contract vector-map
      (case-lambda
        [(p:procedure vec:vector)
          ((typed-lambda (p vec len) ((Callable Object[] Int32) Object)
            (let ((res (clr-cast Object[] (make-vector len '()))))
              (do ((i 0 ($fx+ i 1)))
                  (($fx=? i len) res)
                ($vector-set! res i (p ($vector-ref vec i))))))
             p vec (vector-length vec))]
        [(p:procedure vec1:vector . vecs:vector)
          (define (ref i) (lambda (x) ($vector-ref x i)))
          (let* ((len (vector-length vec1))
                 (res (make-vector len '())))
            (do ((i 0 ($fx+ i 1)))
                (($fx=? i len) res)
              ($vector-set! res i
                    (apply p (map (ref i) 
                                  (cons vec1 vecs))))))]))
          
    (define/contract vector-for-each
      (case-lambda
        [(p:procedure vec:vector)
          ((typed-lambda (p vec len) ((Callable Object[] Int32) Object)
            (do ((i 0 ($fx+ i 1)))
                (($fx=? i len))
              (p ($vector-ref vec i))))
            p vec (vector-length vec))]
        [(p:procedure vec1:vector . vecs:vector)
          (define (ref i) (lambda (x) ($vector-ref x i)))
          (let ((len (vector-length vec1)))
            (do ((i 0 ($fx+ i 1)))
                (($fx=? i len))
              (apply p (map (ref i) 
                            (cons vec1 vecs)))))]))
            
    (define/contract string-for-each
      (case-lambda
        [(p:procedure str:string)
          (let ((len (string-length str)))
            (do ((i 0 ($fx+ i 1)))
                (($fx=? i len))
              (p (string-ref str i))))]
        [(p:procedure str1:string . strs:string)
          (define (ref i) (lambda (x) (string-ref x i)))
          (let ((len (string-length str1)))
            (do ((i 0 ($fx+ i 1)))
                (($fx=? i len))
              (apply p (map (ref i) 
                            (cons str1 strs)))))]))

)
