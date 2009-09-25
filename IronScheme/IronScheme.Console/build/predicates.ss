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
    procedure?)
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
      procedure?)
    (ironscheme unsafe)
    (ironscheme clr))
    
  (define (fixnum? obj)
    (clr-is Int32 obj))     
    
  (define (flonum? obj)
    (clr-is Double obj))     

  (define (bignum? obj)
    (clr-is Microsoft.Scripting.Math.BigInteger obj))
    
  (define (rectnum? obj)
    (clr-is IronScheme.Runtime.ComplexFraction obj)) 
  
  (define (ratnum? obj)
    (clr-is IronScheme.Runtime.Fraction obj))
  
  (define (complexnum? obj)
    (clr-is Microsoft.Scripting.Math.Complex64 obj))
    
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
    (clr-is System.Text.StringBuilder obj))
    
  (define (string? obj)
    (or (clr-string? obj) 
        (stringbuilder? obj)))             

  (define (char? obj)
    (clr-is Char obj))
    
  (define (vector? obj)
    (clr-is Object[] obj))

  (define (bytevector? obj)
    (clr-is Byte[] obj))

  (define (symbol? obj)
    (clr-is Microsoft.Scripting.SymbolId obj))
    
  (define (boolean? obj)
    (clr-is Boolean obj))
   
  (define (procedure? obj)
    (clr-is IronScheme.Runtime.Callable obj))  
                          
   
)
  
   