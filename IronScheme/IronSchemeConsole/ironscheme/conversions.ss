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
    (ironscheme clr))

  (clr-using system)

  (define (->byte obj)
    (clr-static-call convert tobyte obj))
    
  (define (->sbyte obj)
    (clr-static-call convert tosbyte obj))
    
  (define (->char obj)
    (clr-static-call convert tochar obj))
    
  (define (->int16 obj)
    (clr-static-call convert toint16 obj))
    
  (define (->uint16 obj)
    (clr-static-call convert touint16 obj))
    
  (define (->int32 obj)
    (clr-static-call convert toint32 obj))
    
  (define (->uint32 obj)
    (clr-static-call convert touint32 obj))
    
  (define (->int64 obj)
    (clr-static-call convert toint64 obj))
    
  (define (->uint64 obj)
    (clr-static-call convert touint64 obj))
    
  (define (->single obj)
    (clr-static-call convert tosingle obj))
    
  (define (->double obj)
    (clr-static-call convert todouble obj))
    
  (define (->decimal obj)
    (clr-static-call convert todecimal obj))
    
  (define (->boolean obj)
    (clr-static-call convert toboolean obj))
  
  (define (->datetime obj)
    (clr-static-call convert todatetime obj))
          
    
  (clr-clear-usings)
)