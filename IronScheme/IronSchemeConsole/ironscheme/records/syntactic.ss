(library (ironscheme records syntactic)
  (export
    define-record-type
    fields
    mutable
    immutable
    parent
    protocol
    sealed
    opaque
    nongenerative
    parent-rtd
    record-type-descriptor
    record-constructor-descriptor)
    
  (import (rnrs records syntactic))
)