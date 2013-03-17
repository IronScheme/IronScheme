#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

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
            (syntax->datum s)))))))