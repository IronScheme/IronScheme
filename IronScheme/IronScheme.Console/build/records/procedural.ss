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
    (ironscheme contracts)
    (ironscheme unsafe))
    
  (clr-using IronScheme.Runtime.R6RS)
  
  (define (record-type-descriptor? obj)
    (clr-is RecordTypeDescriptor obj))
    
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
    
)