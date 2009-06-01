(library (ironscheme records inspection)
  (export
    record?
    record-rtd
    record-type-name
    record-type-parent
    record-type-uid
    record-type-generative?
    record-type-sealed?
    record-type-opaque?
    record-type-field-names
    record-field-mutable?)
  (import (except (ironscheme)
            record-type-name
            record-type-parent
            record-type-uid
            record-type-generative?
            record-type-sealed?
            record-type-opaque?
            record-type-field-names
            record-field-mutable?)
          (ironscheme clr))
          
  (clr-using IronScheme.Runtime.R6RS)          
          
  (define (record-type-name rtd)
    (string->symbol (clr-prop-get RecordTypeDescriptor Name rtd)))
    
  (define (record-type-parent rtd)
    (let ((p (clr-prop-get RecordTypeDescriptor Parent rtd)))
      (if (null? p)
          #f
          p)))
          
  (define (record-type-uid rtd)
    (clr-field-get RecordTypeDescriptor uid rtd))
    
  (define (record-type-generative? rtd)
    (clr-prop-get RecordTypeDescriptor Generative rtd))    
    
  (define (record-type-sealed? rtd)
    (clr-prop-get RecordTypeDescriptor Sealed rtd))    

  (define (record-type-opaque? rtd)
    (clr-prop-get RecordTypeDescriptor Opaque rtd))    

  (define (record-type-field-names rtd)
    (vector-map (lambda (fd)
                  (string->symbol (clr-prop-get FieldDescriptor Name fd)))
                (clr-prop-get RecordTypeDescriptor Fields rtd)))

  (define (record-field-mutable? rtd k)
    (clr-prop-get FieldDescriptor 
                  Mutable 
                  (vector-ref (clr-prop-get RecordTypeDescriptor Fields rtd) 
                              k)))
    
)