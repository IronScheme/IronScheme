#| License
Copyright (c) 2007-2016 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme misc)
  (export 
    procedure-arity
    procedure-name
    procedure-form
    procedure-environment
    make-guid
    typeof
    clr-type?
    set-symbol-value!
    gc-collect
    vector-reverse!
    vector-copy
    vector-contains?
    vector-index-of
    vector-binary-search
    )
  (import 
    (rnrs)
    (ironscheme contracts)
    (ironscheme typed)
    (ironscheme clr))
  
  (clr-using IronScheme.Runtime)
  
  (define/contract (procedure-arity proc:procedure)
    (clr-prop-get Callable Arity proc))
    
  (define/contract (procedure-name proc:procedure)
    (string->symbol (clr-call Object ToString proc)))

  (define/contract (procedure-form proc:procedure)
    (clr-prop-get Callable Form proc))

  (define/contract (procedure-environment proc:procedure)
    #f)
    
  (define (make-guid)
    (clr-static-call Guid NewGuid))
  
  (define (typeof obj -> Type)
    (if (null? obj)
        (clr-type-of Object)
        (clr-call Object GetType obj))) 
        
  (define: (clr-type? o -> bool)
    (clr-is Type o))
    
  (define (set-symbol-value! symbol value)
    (clr-static-call Builtins SetSymbolValueFast symbol value))
    
  (define (gc-collect)
    (clr-static-call GC Collect)) 
    
  (define/contract (vector-binary-search vec:vector obj)
    (clr-static-call Array BinarySearch vec obj))
  
  (define/contract (vector-index-of vec:vector obj)
    (clr-static-call Array IndexOf vec obj))
    
  (define/contract (vector-contains? vec:vector obj)
    (fx>=? (clr-static-call Array IndexOf vec obj) 0))
    
  (define/contract (vector-copy vec:vector)
    (clr-call Array Clone vec))
    
  (define/contract (vector-reverse! vec:vector)
    (clr-static-call Array Reverse vec)))      