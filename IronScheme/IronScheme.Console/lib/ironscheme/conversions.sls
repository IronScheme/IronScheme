#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme conversions)
  (export
    ->byte
    ->sbyte
    ->char
    ->int16
    ->uint16
    ->int32
    ->uint32
    ->int64
    ->uint64
    ->single
    ->double
    ->decimal
    ->boolean
    ->datetime)
  (import 
    (rnrs)
    (ironscheme typed)
    (ironscheme clr))

  (define: (->byte obj) : Byte
    (clr-static-call Convert ToByte obj))
    
  (define: (->sbyte obj) : SByte
    (clr-static-call Convert ToSByte obj))
    
  (define: (->char obj) : Char
    (clr-static-call Convert ToChar obj))
    
  (define: (->int16 obj) : Int16
    (clr-static-call Convert ToInt16 obj))
    
  (define: (->uint16 obj) : UInt16
    (clr-static-call Convert ToUInt16 obj))
    
  (define: (->int32 obj) : Int32
    (clr-static-call Convert ToInt32 obj))
    
  (define: (->uint32 obj) : UInt32
    (clr-static-call Convert ToUInt32 obj))
    
  (define: (->int64 obj) : Int64
    (clr-static-call Convert ToInt64 obj))
    
  (define: (->uint64 obj) : UInt64
    (clr-static-call Convert ToUInt64 obj))
    
  (define: (->single obj) : Single
    (clr-static-call Convert ToSingle obj))
    
  (define: (->double obj) : Double
    (clr-static-call Convert ToDouble obj))
    
  (define: (->decimal obj) : Decimal
    (clr-static-call Convert ToDecimal obj))
    
  (define: (->boolean obj) : Boolean
    (clr-static-call Convert ToBoolean obj))
  
  (define: (->datetime obj) : DateTime
    (clr-static-call Convert ToDateTime obj)))