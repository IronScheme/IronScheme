(library (ironscheme strings)
  (export 
    string-split
    string-index-of
    string-contains?
    string-starts-with?
    string-ends-with?
    string-ci-contains?
    string-ci-starts-with?
    string-ci-ends-with?    
    ;string-trim-start
    ;string-trim-end
    string-trim
    string-join
    )
  (import 
    (rnrs)
    (ironscheme clr))
    
  (define (string-split str . del)
    (unless (string? str)
      (assertion-violation 'string-split "not a string" str))
    (unless (for-all string? del)
      (assertion-violation 'string-split "not a list of strings" del))
    (clr-call system.string split str (clr-cast system.string[] (list->vector del)) 'none))  
    
  (define (string-join del strs)
    (unless (string? del)
      (assertion-violation 'string-join "not a string" del))
    (unless (for-all string? strs)
      (assertion-violation 'string-join "not a list of strings" strs))  
    (clr-static-call system.string join del (list->vector strs)))  
          
  (define string-index-of
    (case-lambda
      [(str sub)    (clr-call system.string indexof str (clr-cast system.string sub))]
      [(str sub k)  (clr-call system.string indexof str (clr-cast system.string sub) (clr-cast system.int32 k))]))
    
  (define (string-contains? str sub)
    (unless (string? str)
      (assertion-violation 'string-contains? "not a string" str))  
    (unless (string? sub)
      (assertion-violation 'string-contains? "not a string" sub))  
    (clr-call system.string contains str sub))
    
  (define (string-ci-contains? str sub)
    (unless (string? str)
      (assertion-violation 'string-ci-contains? "not a string" str))  
    (unless (string? sub)
      (assertion-violation 'string-ci-contains? "not a string" sub))      
    (clr-call system.string contains (string-upcase str) (string-upcase sub)))

  (define (string-starts-with? str sub)
    (unless (string? str)
      (assertion-violation 'string-starts-with? "not a string" str))  
    (unless (string? sub)
      (assertion-violation 'string-starts-with? "not a string" sub))  
    (clr-call system.string startswith str sub))

  (define (string-ci-starts-with? str sub)
    (unless (string? str)
      (assertion-violation 'string-ci-starts-with? "not a string" str))  
    (unless (string? sub)
      (assertion-violation 'string-ci-starts-with? "not a string" sub))  
    (clr-call system.string startswith (string-upcase str) (string-upcase sub)))

  (define (string-ends-with? str sub)
    (unless (string? str)
      (assertion-violation 'string-ends-with? "not a string" str))  
    (unless (string? sub)
      (assertion-violation 'string-ends-with? "not a string" sub))  
    (clr-call system.string endswith str sub))

  (define (string-ci-ends-with? str sub)
    (unless (string? str)
      (assertion-violation 'string-ci-ends-with? "not a string" str))  
    (unless (string? sub)
      (assertion-violation 'string-ci-ends-with? "not a string" sub))  
    (clr-call system.string endswith (string-upcase str) (string-upcase sub)))

  (define (string-trim str)
    (unless (string? str)
      (assertion-violation 'string-trim "not a string" str))  
    (clr-call system.string trim str))

          
)
  