(library (ironscheme regex)
  (export
    regex-match?
    ;; regex-match ; do this some time
    regex-split
    regex-replace
    regex-escape
    regex-unescape)
  (import 
    (rnrs)
    (ironscheme clr))
    
  (clr-using system.text.regularexpressions)
  
  (define (regex-match? input pattern)
    (clr-static-call regex ismatch input pattern))

  (define (regex-split input pattern)
    (clr-static-call regex split input pattern))
    
  (define (regex-replace input pattern replacement)
    (clr-static-call regex replace input pattern (clr-cast system.string replacement))) ; need cast to deal with overload

  (define (regex-escape input)
    (clr-static-call regex escape input))

  (define (regex-unescape input)
    (clr-static-call regex unescape input))
    
    
  (clr-clear-usings)    
)
    