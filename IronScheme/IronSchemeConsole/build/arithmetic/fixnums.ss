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
      fx*/carry
      fx-/carry
      fx+/carry
      ))
      
  (define (fx*/carry fx1 fx2 fx3)
    (let ((s (+ (* fx1 fx2) fx3))
          (e (expt 2 (fixnum-width))))
      (values (mod0 s e) (div0 s e))))

  (define (fx-/carry fx1 fx2 fx3)
    (let ((s (- fx1 fx2 fx3))
          (e (expt 2 (fixnum-width))))
      (values (mod0 s e) (div0 s e))))

  (define (fx+/carry fx1 fx2 fx3)
    (let ((s (+ fx1 fx2 fx3))
          (e (expt 2 (fixnum-width))))
      (values (mod0 s e) (div0 s e))))
  
  (define (fxif fx1 fx2 fx3)
    (fxior (fxand fx1 fx2)
      (fxand (fxnot fx1) fx3)))
     
  (define (fxcopy-bit fx1 fx2 fx3)      
    (fxif (fxarithmetic-shift-left 1 fx2)
      (fxarithmetic-shift-left fx3 fx2) fx1))
        
  (define (fxbit-field fx1 fx2 fx3)
    (fxarithmetic-shift-right 
      (fxand fx1 (fxnot (fxarithmetic-shift-left -1 fx3)))
      fx2))
        
  (define (fxcopy-bit-field to start end from)
    (fxif 
      (fxand 
        (fxarithmetic-shift-left -1 start) 
        (fxnot (fxarithmetic-shift-left -1 end)))
      (fxarithmetic-shift-left from start)
      to))
              
  (define (fxarithmetic-shift-left fx1 fx2)
    (fxarithmetic-shift fx1 fx2))            
    
  (define (fxarithmetic-shift-right fx1 fx2)
    (fxarithmetic-shift fx1 (fx- fx2)))            
          
  (define (fxrotate-bit-field n start end count)
    (let ((width (fx- end start)))
      (if (fxpositive? width)
        (let ((count (fxmod count width))
              (field (fxbit-field n start end)))
           (fxcopy-bit-field n start end 
            (fxior 
              (fxarithmetic-shift-left field count) 
              (fxarithmetic-shift-right field (fx- width count)))))
        n)))
)