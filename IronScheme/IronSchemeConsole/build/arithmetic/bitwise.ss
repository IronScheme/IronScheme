(library (ironscheme arithmetic bitwise)
  (export
    bitwise-not
    
    bitwise-and
    bitwise-ior
    bitwise-xor
    
    bitwise-if
    
    bitwise-bit-count
    bitwise-length
    bitwise-bit-field
    
    bitwise-first-bit-set
    bitwise-bit-set?
    
    bitwise-copy-bit
    bitwise-copy-bit-field
    
    bitwise-arithmetic-shift
    bitwise-arithmetic-shift-left
    bitwise-arithmetic-shift-right
    
    bitwise-rotate-bit-field
    bitwise-reverse-bit-field)
    
  (import 
    (except (rnrs) bitwise-if bitwise-copy-bit bitwise-bit-field bitwise-copy-bit-field
                   bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
                   bitwise-rotate-bit-field))
  
  (define (bitwise-if ei1 ei2 ei3)
    (bitwise-ior (bitwise-and ei1 ei2)
      (bitwise-and (bitwise-not ei1) ei3)))
      
  (define (bitwise-copy-bit ei1 ei2 ei3)
    (bitwise-if 
      (bitwise-arithmetic-shift-left 1 ei2)
      (bitwise-arithmetic-shift-left ei3 ei2)
      ei1))
        
  (define (bitwise-bit-field ei1 ei2 ei3)
    (bitwise-arithmetic-shift-right
      (bitwise-and ei1 (bitwise-not (bitwise-arithmetic-shift-left -1 ei3)))
      ei2))
  
  (define (bitwise-copy-bit-field to start end from)
    (bitwise-if 
      (bitwise-and 
        (bitwise-arithmetic-shift-left -1 start) 
        (bitwise-not (bitwise-arithmetic-shift-left -1 end)))
      (bitwise-arithmetic-shift-left from start)
      to))
                  
  (define (bitwise-arithmetic-shift-left ei1 ei2)
    (bitwise-arithmetic-shift ei1 ei2))            
    
  (define (bitwise-arithmetic-shift-right ei1 ei2)
    (bitwise-arithmetic-shift ei1 (- ei2)))            
    
  (define (bitwise-rotate-bit-field n start end count)
    (let ((width (- end start)))
      (if (positive? width)
        (let ((count (mod count width))
              (field (bitwise-bit-field n start end)))
           (bitwise-copy-bit-field n start end 
            (bitwise-ior 
              (bitwise-arithmetic-shift-left field count) 
              (bitwise-arithmetic-shift-right field (- width count)))))
        n)))
)
