(library (ironscheme strings)
  (export 
    string-split
    string-index-of
    string-contains?
    string-starts-with?
    string-ends-with?
    ;string-trim-start
    ;string-trim-end
    string-trim
    
    )
  (import 
    (rnrs)
    (ironscheme clr))
    
  (define (string-split str . del)
    (clr-call system.string split str (clr-cast system.string[] (list->vector del)) 'none))  
    
  (define string-index-of
    (case-lambda
      [(str sub)    (clr-call system.string indexof str (clr-cast system.string sub))]
      [(str sub k)  (clr-call system.string indexof str (clr-cast system.string sub) (clr-cast system.int32 k))]))
    
  (define (string-contains? str sub)
    (clr-call system.string contains str sub))

  (define (string-starts-with? str sub)
    (clr-call system.string startswith str sub))

  (define (string-ends-with? str sub)
    (clr-call system.string endswith str sub))

  (define (string-trim str)
    (clr-call system.string trim str))

          
)
  