#| License
Copyright (c) 2007-2014 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme records procedural)
  (export
    make-record-type-descriptor
    record-type-descriptor?
    make-record-constructor-descriptor
    record-constructor-descriptor?
    record-constructor
    record-predicate
    record-accessor
    record-mutator)
  (import 
    (except (ironscheme) record-type-descriptor? record-constructor-descriptor?)
    (ironscheme clr)
    (ironscheme contracts)
    (ironscheme unsafe))
    
  (clr-using IronScheme.Runtime.R6RS)
  
  (define (record-type-descriptor? obj)
    (clr-is RecordTypeDescriptor obj))
    
  (define (record-constructor-descriptor? obj)
    (clr-is RecordConstructorDescriptor obj))
    
  #|  
  (define/contract (record-predicate rtd:record-type-descriptor)
    (clr-prop-get RecordTypeDescriptor Predicate rtd))       

  (define/contract (record-accessor rtd:record-type-descriptor k)
    (clr-prop-get FieldDescriptor 
                  Accessor 
                  ($vector-ref (clr-prop-get RecordTypeDescriptor Fields rtd) 
                               k)))       

  (define/contract (record-mutator rtd:record-type-descriptor k)
    (clr-prop-get FieldDescriptor 
                  Mutator 
                  ($vector-ref (clr-prop-get RecordTypeDescriptor Fields rtd) 
                               k)))       
 |#   
)