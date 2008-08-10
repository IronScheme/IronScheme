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
    inexact
    exact
    
    =
    <
    >
    <=
    >=
    
    zero?
    positive?
    negative?
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
    
    quasiquote
    unquote
    unquote-splicing
    
    let-syntax
    letrec-syntax
    syntax-rules
    identifier-syntax
    
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
     ))
     
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
    
    
;    (define (div-and-mod x1 x2)
;      (let ((exact-args? (and (exact? x1) (exact? x2)))
;            (scale 1))
;         (when exact-args?
;            (set! scale (* (denominator x1) (denominator x2)))
;            (set! x1 (* scale x1))
;            (set! x2 (* scale x2)))
;         (let ((div (floor (/ x1 x2)))(mod (% x1 x2)))
;           (if (negative? mod)
;            (when (or (and (
;        
;      ))


    (define-syntax make-string-compare
      (syntax-rules ()
        [(_ cmp k)
          (lambda (a b . rest)
            (unless (string? a) (assertion-violation 'k "not a string" a))
            (unless (string? b) (assertion-violation 'k "not a string" b))  
            (for-each (lambda (x)
                        (unless (string? x) (assertion-violation 'k "not a string" x)))
                      rest)  
            (let f ((a a)(b b)(rest rest))                    
              (if (null? rest)
                (cmp (string-compare a b) 0)
                (and 
                  (cmp (string-compare a b) 0)
                  (f b (car rest) (cdr rest))))))]))

    (define string=? (make-string-compare = string=?))
    (define string<? (make-string-compare < string<?))
    (define string>? (make-string-compare > string>?))
    (define string<=? (make-string-compare <= string<=?))
    (define string>=? (make-string-compare >= string>=?))

    (define (symbol=? a b . rest)
      (unless (symbol? a) (assertion-violation 'symbol=? "not a symbol" a))
      (unless (symbol? b) (assertion-violation 'symbol=? "not a symbol" b))
      (for-each (lambda (x)
                  (unless (symbol? x) (assertion-violation 'symbol=? "not a symbol" x)))
                rest) 
      (let f ((a a)(b b)(rest rest))                      
        (if (null? rest)
          (eq? a b)
          (and 
            (eq? a b)
            (f b (car rest) (cdr rest))))))

    (define (boolean=? a b . rest)
      (unless (boolean? a) (assertion-violation 'boolean=? "not a boolean" a))
      (unless (boolean? b) (assertion-violation 'boolean=? "not a boolean" b))
      (for-each (lambda (x)
                  (unless (boolean? x) (assertion-violation 'boolean=? "not a boolean" x)))
                rest)       
      (let f ((a a)(b b)(rest rest))                      
        (if (null? rest)
          (eq? a b)
          (and 
            (eq? a b)
            (f b (car rest) (cdr rest))))))
      
    (define-syntax char-compare
      (syntax-rules ()
        [(_ cmp k)
          (lambda (a b . rest)
            (unless (char? a) (assertion-violation 'k "not a char" a))
            (unless (char? b) (assertion-violation 'k "not a char" b))  
            (for-each (lambda (x)
                        (unless (char? x) (assertion-violation 'k "not a char" x)))
                      rest)  
            (let f ((a a)(b b)(rest rest))                    
              (if (null? rest)
                (cmp (char->integer a) (char->integer b))
                (and 
                  (cmp (char->integer a) (char->integer b))
                  (f b (car rest) (cdr rest))))))]))
    
    (define char=? (char-compare = char=?))
    (define char<? (char-compare < char<?))
    (define char>? (char-compare > char>?))
    (define char<=? (char-compare <= char<=?))
    (define char>=? (char-compare >= char>=?))
    
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

)
