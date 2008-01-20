(library (ironscheme conversions)
  (export
    object->byte
    object->sbyte
    object->char
    object->int16
    object->uint16
    object->int32
    object->uint32
    object->int64
    object->uint64
    object->single
    object->double
    object->decimal
    object->boolean
    object->datetime)
  (import 
    (rnrs)
    (ironscheme clr))

  (clr-using system)

  (define (object->byte obj)
    (clr-static-call convert tobyte obj))
    
  (define (object->sbyte obj)
    (clr-static-call convert tosbyte obj))
    
  (define (object->char obj)
    (clr-static-call convert tochar obj))
    
  (define (object->int16 obj)
    (clr-static-call convert toint16 obj))
    
  (define (object->uint16 obj)
    (clr-static-call convert touint16 obj))
    
  (define (object->int32 obj)
    (clr-static-call convert toint32 obj))
    
  (define (object->uint32 obj)
    (clr-static-call convert touint32 obj))
    
  (define (object->int64 obj)
    (clr-static-call convert toint64 obj))
    
  (define (object->uint64 obj)
    (clr-static-call convert touint64 obj))
    
  (define (object->single obj)
    (clr-static-call convert tosingle obj))
    
  (define (object->double obj)
    (clr-static-call convert todouble obj))
    
  (define (object->decimal obj)
    (clr-static-call convert todecimal obj))
    
  (define (object->boolean obj)
    (clr-static-call convert toboolean obj))
  
  (define (object->datetime obj)
    (clr-static-call convert todatetime obj))
          
    
  (clr-clear-usings)
)