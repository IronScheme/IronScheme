(library (ironscheme syntax-format)
  (export
    syntax-format)
  (import 
    (rnrs)
    (ironscheme format))
  
  (define (syntax-format fmt loc . args)
    (datum->syntax loc 
      (string->symbol
        (apply format fmt (map syntax->datum args)))))

)
