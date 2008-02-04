(library (ironscheme collections icollection)
  (export
    icollection?
    icollection-count)
  (import 
    (rnrs)
    (ironscheme clr))

  (clr-using system.collections)
     
  (define (icollection? o)
    (clr-is icollection o))

  (define (icollection-count s)
    (clr-prop-get icollection count s))

    
  (clr-clear-usings)
)