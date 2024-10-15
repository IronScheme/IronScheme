#| License
Copyright (c) 2007-2016 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme predicates)
  (export
    bignum?
    rectnum?
    ratnum?
    complexnum?
    flonum?
    fixnum?
    number?
    stringbuilder?
    string?
    clr-string?
    char?
    vector?
    bytevector?
    symbol?
    boolean?
    procedure?
    condition?
    valuetype-vector?)
  (import 
    (except 
      (rnrs)
      fixnum?
      flonum?
      number?
      string?
      char?
      vector?
      bytevector?
      symbol?
      boolean?
      procedure?
      condition?)
    (ironscheme unsafe)
    (ironscheme clr))
    
  (clr-using IronScheme.Runtime)   
  (clr-using Microsoft.Scripting) 
  (clr-using Microsoft.Scripting.Math)
  (clr-using Oyster.Math)
  (clr-using System.Text)
  (clr-using IronScheme.Scripting) 
  (clr-using IronScheme.Scripting.Math)
  
  (define (valuetype-vector? obj)
    (and (clr-is Array obj)
         (let ((t (clr-call Type GetElementType (clr-call Object GetType obj))))
          (clr-prop-get Type IsValueType t))))
  
  (define (condition? obj)
    (clr-is IronScheme.Runtime.R6RS.Condition obj))
    
  (define (fixnum? obj)
    (clr-is Int32 obj))
    
  (define (flonum? obj)
    (clr-is Double obj))

  (define (bignum? obj)
    (clr-is IntX obj))
    
  (define (rectnum? obj)
    (clr-is ComplexFraction obj)) 
  
  (define (ratnum? obj)
    (clr-is Fraction obj))
  
  (define (complexnum? obj)
    (clr-is Complex64 obj))
    
  (define (number? obj)
    ($or? (fixnum? obj)
          (flonum? obj)
          (bignum? obj)
          (ratnum? obj)
          (complexnum? obj)
          (rectnum? obj))) 

  (define (clr-string? obj)
    (clr-is String obj))

  (define (stringbuilder? obj)
    (clr-is StringBuilder obj))
    
  (define (string? obj)
    (or (clr-string? obj) 
        (stringbuilder? obj)))

  (define (char? obj)
    (or (clr-is Char obj)
        (clr-is SchemeChar obj)))

  (define (vector? obj)
    (clr-is Object[] obj))

  (define (bytevector? obj)
    (clr-is Byte[] obj))

  (define (symbol? obj)
    (clr-is SymbolId obj))
    
  (define (boolean? obj)
    (clr-is Boolean obj))
   
  (define (procedure? obj)
    (clr-is Callable obj)))
