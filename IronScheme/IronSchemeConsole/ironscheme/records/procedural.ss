(library (ironscheme records procedural (6))
  (export
    make-record-type-descriptor
    record-type-descriptor?
    make-record-constructor-descriptor
    record-constructor
    record-predicate
    record-accessor
    record-mutator)
    
  (import (rnrs))
)