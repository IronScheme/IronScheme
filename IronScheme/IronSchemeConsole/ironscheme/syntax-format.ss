(library (ironscheme syntax-format)
  (export
    syntax-format)
  (import 
    (rnrs)
    (ironscheme format))
  
  (define (syntax-format fmt arg1 . args)
    (datum->syntax arg1 
      (string->symbol
        (apply format fmt
          (syntax->datum arg1)
          (map syntax->datum args)))))

)