#| License
Copyright (c) 2011 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme typed parsing-helper)
  (export
    ->
    parse-type)
  (import 
    (ironscheme))

  (define-syntax ->
    (lambda (x)
      (syntax-violation '-> "invalid usage of auxilliary keyword" x)))    
    
  (define (parse-type x)
    (syntax-case x (->)
      [(a ... -> r)
        (with-syntax (((a ...) (map parse-type #'(a ...)))
                      (r (parse-type #'r)))
          #'(IronScheme.Runtime.Typed.TypedClosure a ... r))]
      [(t a ...)
        (with-syntax (((a ...) (map parse-type #'(a ...))))
          #'(t a ...))]
      [t #'t])))    
