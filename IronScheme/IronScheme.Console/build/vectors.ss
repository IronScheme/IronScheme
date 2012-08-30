(library (ironscheme vectors)
  (export
    flvector?
    fxvector?
    flvector-ref
    fxvector-ref
    flvector-set!
    fxvector-set!
    make-flvector
    make-fxvector
    flvector-length
    fxvector-length
    )
    
  (import 
    (rnrs)
    (only (ironscheme) void)
    (ironscheme unsafe)
    (ironscheme clr)
    (ironscheme typed))
      
    (define (flvector? obj)
      (clr-is Double[] obj))    
      
    (define (fxvector? obj)
      (clr-is Int32[] obj))    

    (define: (flvector-ref (x : Double[]) (n : Int32)) : Double
      (when ($fx>? 0 n)
        (assertion-violation 'flvector-ref "negative index" n))
      ($vector-ref x n))

    (define: (fxvector-ref (x : Int32[]) (n : Int32)) : Int32
      (when ($fx>? 0 n)
        (assertion-violation 'fxvector-ref "negative index" n))
      ($vector-ref x n))
      
    (define: (flvector-set! (x : Double[]) (n : Int32) (value : Double))
      (when ($fx>? 0 n)
        (assertion-violation 'flvector-set! "negative index" n))
      ($vector-set! x n value)
      (void))

    (define: (fxvector-set! (x : Int32[]) (n : Int32) (value : Int32))
      (when ($fx>? 0 n)
        (assertion-violation 'flvector-set! "negative index" n))
      ($vector-set! x n value)
      (void))
     
    (define: (make-flvector (k : Int32)) : Double[]
      (when ($fx>? 0 k)
        (assertion-violation 'make-flvector "cannot be negative" k))
      (clr-new-array Double k))
      
    (define: (make-fxvector (k : Int32)) : Int32[]
      (when ($fx>? 0 k)
        (assertion-violation 'make-fxvector "cannot be negative" k))
      (clr-new-array Int32 k))
      
    (define: (flvector-length (vec : Double[])) : Int32
      (clr-prop-get Array Length vec))
      
    (define: (fxvector-length (vec : Int32[])) : Int32
      (clr-prop-get Array Length vec))              
      
)
