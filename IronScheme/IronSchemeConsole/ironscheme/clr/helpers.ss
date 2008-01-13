(library (ironscheme clr helpers)
  (export 
    symbol->syntax
    prefix-syntax)
  (import 
    (rnrs)
    (only (psyntax system $bootstrap) gensym))
  
  (define (symbol->syntax s n)
    (datum->syntax s (gensym n)))
    
  (define (prefix-syntax h s)
    (datum->syntax s 
      (string->symbol 
        (string-append h 
          (symbol->string 
            (syntax->datum s))))))
  
  )