#| ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 2007,2008,2009
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************|#

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