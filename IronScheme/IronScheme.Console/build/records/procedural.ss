(library (ironscheme records procedural)
  (export
    make-record-type-descriptor
    record-type-descriptor?
    make-record-constructor-descriptor
    record-constructor
    record-predicate
    record-accessor
    record-mutator)
    
  (import 
    (except (ironscheme) 
      record-type-descriptor? record-predicate
      record-accessor record-mutator)
    (ironscheme clr)
    (ironscheme unsafe))
    
  (clr-using IronScheme.Runtime.R6RS)
  
  (define (record-type-descriptor? obj)
    (clr-is RecordTypeDescriptor obj))
    
  (define (record-predicate rtd)
    (clr-prop-get RecordTypeDescriptor Predicate rtd))       

  (define (record-accessor rtd k)
    (clr-prop-get FieldDescriptor 
                  Accessor 
                  ($vector-ref (clr-prop-get RecordTypeDescriptor Fields rtd) 
                               k)))       

  (define (record-mutator rtd k)
    (clr-prop-get FieldDescriptor 
                  Mutator 
                  ($vector-ref (clr-prop-get RecordTypeDescriptor Fields rtd) 
                               k)))       
    
)