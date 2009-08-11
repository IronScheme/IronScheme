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
          (ironscheme contracts)
          (ironscheme clr))
          
  (clr-using IronScheme.Runtime.R6RS)          
          
  (define/contract (record-type-name rtd:record-type-descriptor)
    (string->symbol (clr-prop-get RecordTypeDescriptor Name rtd)))
    
  (define/contract (record-type-parent rtd:record-type-descriptor)
    (let ((p (clr-prop-get RecordTypeDescriptor Parent rtd)))
      (if (null? p)
          #f
          p)))
          
  (define/contract (record-type-uid rtd:record-type-descriptor)
    (clr-field-get RecordTypeDescriptor uid rtd))
    
  (define/contract (record-type-generative? rtd:record-type-descriptor)
    (clr-prop-get RecordTypeDescriptor Generative rtd))    
    
  (define/contract (record-type-sealed? rtd:record-type-descriptor)
    (clr-prop-get RecordTypeDescriptor Sealed rtd))    

  (define/contract (record-type-opaque? rtd:record-type-descriptor)
    (clr-prop-get RecordTypeDescriptor Opaque rtd))    

  (define/contract (record-type-field-names rtd:record-type-descriptor)
    (vector-map (lambda (fd)
                  (string->symbol (clr-prop-get FieldDescriptor Name fd)))
                (clr-prop-get RecordTypeDescriptor Fields rtd)))

  (define/contract (record-field-mutable? rtd:record-type-descriptor k:fixnum)
    (clr-prop-get FieldDescriptor 
                  Mutable 
                  (vector-ref (clr-prop-get RecordTypeDescriptor Fields rtd) 
                              k)))
    
)