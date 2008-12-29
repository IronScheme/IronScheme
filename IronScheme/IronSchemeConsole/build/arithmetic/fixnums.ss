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
    (ironscheme unsafe)
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
      
      fxmod
      fxmod0
      fxdiv-and-mod
      fxdiv0-and-mod0
      
      fxzero?
      fxpositive?
      fxnegative?
      fxodd?
      fxeven?
      
      fxmax
      fxmin
      ))
      
  (define-syntax define-fx
    (lambda (x)
      (syntax-case x ()
        [(_ (name formals ...) body body* ...)
          (with-syntax (((checks ...) 
            (map (lambda (f)
                   (with-syntax ((f f))
                     #'(unless (fixnum? f) 
                        (assertion-violation 'name "not a fixnum" f))))
                  #'(formals ...))))
            #'(define (name formals ...)
                checks ...
                (let ()
                  body body* ...)))])))
      
  (define-fx (fxmod x1 x2)
    ($$fx- x1 ($$fx* (fxdiv x1 x2) x2)))

  (define-fx (fxmod0 x1 x2)
    ($$fx- x1 ($$fx* (fxdiv0 x1 x2) x2)))
    
  (define-fx (fxdiv-and-mod x1 x2)
    (let ((d (fxdiv x1 x2)))
      (values d ($$fx- x1 ($$fx* d x2)))))             

  (define-fx (fxdiv0-and-mod0 x1 x2)
    (let ((d (fxdiv0 x1 x2)))
      (values d ($fx- x1 ($fx* d x2))))) 
      
  (define-fx (fxpositive? r)
    ($fx<? 0 r))
    
  (define-fx (fxnegative? r)
    ($fx>? 0 r))   
    
  (define-fx (fxzero? r)
    ($fx=? 0 r))           
    
  (define-fx (fxeven? n)
    (if ($fx=? n (least-fixnum))
      #t      
      ($fx=? 0 (fxmod n 2))))

  (define-fx (fxodd? n)
    (if ($fx=? n (least-fixnum))
      #f      
      ($fx=? 1 (fxmod n 2))))      
  
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
      
  (define-fx (fx*/carry fx1 fx2 fx3)
    (let ((s (+ (* fx1 fx2) fx3))
          (e (expt 2 (fixnum-width))))
      (values (mod0 s e) (div0 s e))))

  (define-fx (fx-/carry fx1 fx2 fx3)
    (let ((s (- fx1 fx2 fx3))
          (e (expt 2 (fixnum-width))))
      (values (mod0 s e) (div0 s e))))

  (define-fx (fx+/carry fx1 fx2 fx3)
    (let ((s (+ fx1 fx2 fx3))
          (e (expt 2 (fixnum-width))))
      (values (mod0 s e) (div0 s e))))
  
  (define-fx (fxif fx1 fx2 fx3)
    ($fxior ($fxand fx1 fx2)
      ($fxand ($$fxnot fx1) fx3)))
     
  (define-fx (fxcopy-bit fx1 fx2 fx3)      
    (fxif (fxarithmetic-shift-left 1 fx2)
      (fxarithmetic-shift-left fx3 fx2) fx1))
        
  (define-fx (fxbit-field fx1 fx2 fx3)
    (fxarithmetic-shift-right 
      ($fxand fx1 (fxnot (fxarithmetic-shift-left -1 fx3)))
      fx2))
        
  (define-fx (fxcopy-bit-field to start end from)
    (fxif 
      ($fxand 
        (fxarithmetic-shift-left -1 start) 
        ($$fxnot (fxarithmetic-shift-left -1 end)))
      (fxarithmetic-shift-left from start)
      to))
              
  (define-fx (fxarithmetic-shift-left fx1 fx2)
    (fxarithmetic-shift fx1 fx2))            
    
  (define-fx (fxarithmetic-shift-right fx1 fx2)
    (fxarithmetic-shift fx1 ($$fx- fx2)))            
          
  (define-fx (fxrotate-bit-field n start end count)
    (let ((width ($$fx- end start)))
      (if (fxpositive? width)
        (let ((count (fxmod count width))
              (field (fxbit-field n start end)))
           (fxcopy-bit-field n start end 
            ($fxior 
              (fxarithmetic-shift-left field count) 
              (fxarithmetic-shift-right field ($$fx- width count)))))
        n)))
)