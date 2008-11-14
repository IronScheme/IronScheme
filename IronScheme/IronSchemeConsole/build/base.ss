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
      max
      min
      positive?
      negative?
      zero?
      even?
      odd?
      gcd
      lcm
      vector-map
      vector-for-each
      string-for-each
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
    
    (define (positive? r)
      (unless (real-valued? r)
        (assertion-violation 'positive? "not a real" r))
      (< 0 r))
      
    (define (negative? r)
      (unless (real-valued? r)
        (assertion-violation 'negative? "not a real" r))
      (> 0 r))   
      
    (define (zero? r)
      (unless (real-valued? r)
        (assertion-violation 'zero? "not a real" r))
      (= 0 r))           
      
    (define (even? n)
      (unless (integer-valued? n)
        (assertion-violation 'even? "not a integer" n))
      (= 0 (mod n 2)))           

    (define (odd? n)
      (unless (integer-valued? n)
        (assertion-violation 'odd? "not a integer" n))
      (= 1 (mod n 2)))      
    
    (define (max a . rest)
      (fold-left 
        (lambda (a b) 
          (let ((r (if (< a b) b a)))
            (if (or (inexact? a) (inexact? b))
              (inexact r)
              r)))
        a 
        rest))
      
    (define (min a . rest)
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
            (unless (integer-valued? n)
              (assertion-violation 'gcd "not an integer" n))
            (abs n))]
        [(2)
          (let ((a (car nums))(b (cadr nums)))
            (unless (integer-valued? a)
              (assertion-violation 'gcd "not an integer" a))
            (unless (integer-valued? b)
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
            (unless (integer-valued? n)
              (assertion-violation 'lcm "not an integer" n))
            (abs n))]
        [(2)
          (let ((a (car nums))(b (cadr nums)))
            (unless (integer-valued? a)
              (assertion-violation 'lcm "not an integer" a))
            (unless (integer-valued? b)
              (assertion-violation 'lcm "not an integer" b))
            (if (or (zero? a)(zero? b))
              0
              (abs (* (/ a (gcd a b)) b))))]
        [else
          (fold-left lcm (abs (car nums)) (cdr nums))]))           
    
    (define-syntax define-string-compare
      (syntax-rules ()
        [(_ name cmp)
          (define name
            (lambda (a b . rest)
              (unless (string? a) (assertion-violation 'name "not a string" a))
              (for-all
                (lambda (x)
                  (unless (string? x) (assertion-violation 'name "not a string" x))  
                  (let ((r (cmp (string-compare a x) 0)))
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
             (res (make-vector len)))
        (do ((i 0 (fx+ i 1)))
            ((fx=? i len) res)
          (vector-set! res i            
            (call-with-values 
              (lambda ()
                (apply values
                  (map (lambda (x) 
                         (vector-ref x i)) 
                       (cons vec1 vecs))))
              p)))))
          
    (define (vector-for-each p vec1 . vecs)
      (let ((len (vector-length vec1)))
        (do ((i 0 (fx+ i 1)))
            ((fx=? i len))
          (call-with-values 
            (lambda ()
              (apply values
                (map (lambda (x) 
                       (vector-ref x i)) 
                     (cons vec1 vecs))))
            p))))
            
    (define (string-for-each p str1 . strs)
      (let ((len (string-length str1)))
        (do ((i 0 (fx+ i 1)))
            ((fx=? i len))
          (call-with-values 
            (lambda ()
              (apply values
                (map (lambda (x) 
                       (string-ref x i)) 
                     (cons str1 strs))))
            p))))                  

)
