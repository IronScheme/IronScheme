(library (ironscheme syntax-case (6))
  (export
    make-variable-transformer
    
    syntax-case
    syntax
    
    identifier?
    bound-identifier=?
    free-identifier=?
    
    syntax->datum
    datum->syntax
    
    generate-temporaries
    
    with-syntax
    
    quasisyntax
    unsyntax
    unsyntax-splicing
    
    syntax-violation)
    
  (import (rnrs))
)