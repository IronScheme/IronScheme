#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme syntax utils)
  (export
    syntax-format)
  (import 
    (ironscheme))
  
  (define (syntax-format fmt loc . args)
    (datum->syntax loc 
      (string->symbol
        (apply format fmt (map syntax->datum args))))))
