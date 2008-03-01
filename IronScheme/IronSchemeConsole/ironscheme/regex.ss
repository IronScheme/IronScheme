(library (ironscheme regex)
  (export
    regex?
    match-value
    regex-match
    regex-matches
    regex-match?
    regex-split
    regex-replace
    regex-escape
    regex-unescape)
  (import 
    (rnrs)
    (ironscheme clr))
    
  (clr-using system.text.regularexpressions)
  
  (define (regex? obj)
    (clr-is regex obj))

  (define (regex-match input pattern)
    (clr-static-call regex match input pattern))

  (define (regex-matches input pattern)
    (clr-static-call regex matches input pattern))
    
  (define (match-value match)
    (clr-prop-get match value match))
    
  (define (match-success? match)
    (clr-prop-get match success match))    
  
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
    