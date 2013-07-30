#| License
Copyright (c) 2011 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme typed struct-descriptor)
  (export
    struct-descriptor?
    make-struct-descriptor
    struct-descriptor-name
    struct-descriptor-namespace
    struct-descriptor-rtd
    struct-descriptor-rcd
    struct-descriptor-ctor
    struct-descriptor-predicate
    struct-descriptor-field-names
    struct-descriptor-field-types
    struct-descriptor-accessors
    struct-descriptor-mutators
    struct-descriptor-methods
    struct-descriptor-method-names)
  (import 
    (ironscheme))
    
  (define-record-type struct-descriptor
    (fields name
            namespace
            rtd
            rcd
            ctor
            predicate
            field-names
            field-types
            accessors
            mutators
            methods))
            
  (define (struct-descriptor-method-names sd)
    (map car (struct-descriptor-methods sd))))