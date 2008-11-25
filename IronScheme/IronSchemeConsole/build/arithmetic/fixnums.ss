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
      
      fxdiv
      fxmod
      fxdiv0
      fxmod0
      
      fxzero?
      fxpositive?
      fxnegative?
      fxodd?
      fxeven?
      
      fxmax
      fxmin
      ))
      
  (define (fxdiv fx1 fx2)
    (let-values (((n d) (fxdiv-and-mod fx1 fx2)))
      n))       

  (define (fxdiv0 fx1 fx2)
    (let-values (((n d) (fxdiv0-and-mod0 fx1 fx2)))
      n))       

  (define (fxmod fx1 fx2)
    (let-values (((n d) (fxdiv-and-mod fx1 fx2)))
      d))       

  (define (fxmod0 fx1 fx2)
    (let-values (((n d) (fxdiv0-and-mod0 fx1 fx2)))
      d)) 
      
  (define (fxpositive? r)
    (unless (fixnum? r)
      (assertion-violation 'fxpositive? "not a fixnum" r))
    (fx<? 0 r))
    
  (define (fxnegative? r)
    (unless (fixnum? r)
      (assertion-violation 'fxnegative? "not a fixnum" r))
    (fx>? 0 r))   
    
  (define (fxzero? r)
    (unless (fixnum? r)
      (assertion-violation 'fxzero? "not a fixnum" r))
    (fx=? 0 r))           
    
  (define (fxeven? n)
    (unless (fixnum? n)
      (assertion-violation 'fxeven? "not a fixnum" n))
    (fx=? 0 (fxmod n 2)))           

  (define (fxodd? n)
    (unless (fixnum? n)
      (assertion-violation 'fxodd? "not a fixnum" n))
    (fx=? 1 (fxmod n 2)))      
  
  (define (fxmax a . rest)
    (unless (fixnum? a)
      (assertion-violation 'fxmax "not a fixnum" a))
    (fold-left 
      (lambda (a b) 
        (if (fx<? a b) b a))
      a 
      rest))
    
  (define (fxmin a . rest)
    (unless (fixnum? a)
      (assertion-violation 'fxmin "not a fixnum" a))
    (fold-left 
      (lambda (a b) 
        (if (fx>? a b) b a))
      a 
      rest))                 
      
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