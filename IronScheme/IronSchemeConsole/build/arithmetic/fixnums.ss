(library (ironscheme arithmetic fixnums)
  (export
    fixnum?
    
    fixnum-width
    least-fixnum
    greatest-fixnum
    
    fx=?
    fx>?
    fx<?
    fx>=?
    fx<=?
    
    fxzero?
    fxpositive?
    fxnegative?
    fxodd?
    fxeven?
    
    fxmax
    fxmin
    
    fx+
    fx*
    fx-
    fxdiv-and-mod
    fxdiv
    fxmod
    fxdiv0-and-mod0
    fxdiv0
    fxmod0
    fx+/carry
    fx-/carry
    fx*/carry
    fxnot
    fxand
    fxior
    fxxor
    fxif
    fxbit-count
    fxlength
    fxfirst-bit-set
    fxbit-set?
    fxcopy-bit
    fxbit-field
    fxcopy-bit-field
    fxarithmetic-shift
    fxarithmetic-shift-left
    fxarithmetic-shift-right
    fxrotate-bit-field
    fxreverse-bit-field
  )
  (import 
    (except (rnrs) 
      fxif
      fxcopy-bit
      fxbit-field
      fxcopy-bit-field
      fxarithmetic-shift-left
      fxarithmetic-shift-right
      fxrotate-bit-field
      ))
  
  (define (fxif fx1 fx2 fx3)
    (fxior (fxand fx1 fx2)
      (fxand (fxnot fx1) fx3)))
     
  (define (fxcopy-bit fx1 fx2 fx3)      
    (let* ((mask (fxarithmetic-shift-left 1 fx2)))
      (fxif mask
        (fxarithmetic-shift-left fx3 fx2)
        fx1)))
        
  (define (fxbit-field fx1 fx2 fx3)
    (let* ((mask (fxnot
       (fxarithmetic-shift-left -1 fx3))))
      (fxarithmetic-shift-right (fxand fx1 mask)
        fx2)))        
        
   (define (fxcopy-bit-field fx1 fx2 fx3 fx4)
     (let* ((to fx1)
            (start fx2)
            (end fx3)
            (from fx4)
            (mask1 (fxarithmetic-shift-left -1 start))
            (mask2 (fxnot
              (fxarithmetic-shift-left -1 end)))
            (mask (fxand mask1 mask2)))
              (fxif mask
                (fxarithmetic-shift-left from start)
                to)))  
                
    (define (fxarithmetic-shift-left fx1 fx2)
      (fxarithmetic-shift fx1 fx2))            
      
    (define (fxarithmetic-shift-right fx1 fx2)
      (fxarithmetic-shift fx1 (- fx2)))            
      
      
    (define (fxrotate-bit-field fx1 fx2 fx3 fx4)
      (let* ((n fx1)
             (start fx2)
             (end fx3)
             (count fx4)
             (width (- end start)))
        (if (positive? width)
          (let* ((count (mod count width))
                 (field0 (fxbit-field n start end))
                 (field1 (fxarithmetic-shift-left field0 count))
                 (field2 (fxarithmetic-shift-right field0 (- width count)))
                 (field (fxior field1 field2)))
             (fxcopy-bit-field n start end field))
          n)))                  
)