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
    
  (import 
    (except (rnrs) bitwise-if bitwise-copy-bit bitwise-bit-field bitwise-copy-bit-field
                   bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
                   bitwise-rotate-bit-field bitwise-reverse-bit-field))
  
  (define (bitwise-if ei1 ei2 ei3)
    (bitwise-ior (bitwise-and ei1 ei2)
      (bitwise-and (bitwise-not ei1) ei3)))
      
  (define (bitwise-copy-bit ei1 ei2 ei3)
    (let* ((mask (bitwise-arithmetic-shift-left 1 ei2)))
      (bitwise-if mask
       (bitwise-arithmetic-shift-left ei3 ei2)
        ei1)))
        
  (define (bitwise-bit-field ei1 ei2 ei3)
     (let ((mask
            (bitwise-not
              (bitwise-arithmetic-shift-left -1 ei3))))
        (bitwise-arithmetic-shift-right
          (bitwise-and ei1 mask)
          ei2)))
  
   (define (bitwise-copy-bit-field ei1 ei2 ei3 ei4)
      (let* ((to ei1)
             (start ei2)
             (end ei3)
             (from ei4)
             (mask1
                (bitwise-arithmetic-shift-left -1 start))
             (mask2
                (bitwise-not
                  (bitwise-arithmetic-shift-left -1 end)))
             (mask (bitwise-and mask1 mask2)))
                (bitwise-if mask
                  (bitwise-arithmetic-shift-left from
                    start)
                  to)))
                  
    (define (bitwise-arithmetic-shift-left ei1 ei2)
      (bitwise-arithmetic-shift ei1 ei2))            
      
    (define (bitwise-arithmetic-shift-right ei1 ei2)
      (bitwise-arithmetic-shift ei1 (- ei2)))            
      
      
    (define (bitwise-rotate-bit-field ei1 ei2 ei3 ei4)
      (let* ((n ei1)
             (start ei2)
             (end ei3)
             (count ei4)
             (width (- end start)))
        (if (positive? width)
          (let* ((count (mod count width))
                 (field0 (bitwise-bit-field n start end))
                 (field1 (bitwise-arithmetic-shift-left field0 count))
                 (field2 (bitwise-arithmetic-shift-right field0 (- width count)))
                 (field (bitwise-ior field1 field2)))
             (bitwise-copy-bit-field n start end field))
          n)))
          
     (define (bitwise-reverse-bit-field ei1 ei2 ei3)
        'TODO)
)
