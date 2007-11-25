(library (ironscheme base (6))
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
  (import (rnrs))
)
