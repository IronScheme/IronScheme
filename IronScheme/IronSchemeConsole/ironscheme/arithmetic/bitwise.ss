(library (ironscheme arithmetic bitwise (6))
  (export
    bitwise-not
    
    bitwise-and
    bitwise-ior
    bitwise-xor
    
    bitwise-if
    
    bitwise-bit-count
    bitwise-length
    
    bitwise-first-bit-set
    bitwise-bit-set?
    
    bitwise-copy-bit
    bitwise-copy-bit-field
    
    bitwise-arithmetic-shift
    bitwise-arithmetic-shift-left
    bitwise-arithmetic-shift-right
    
    bitwise-rotate-bit-field
    bitwise-reverse-bit-field)
    
  (import (rnrs))
)