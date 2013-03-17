#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme regex)
  (export
    regex?
    make-regex
    group-value
    match-group
    group-success?
    regex-match
    regex-matches
    regex-match?
    regex-split
    regex-replace
    regex-escape
    regex-unescape)
  (import 
    (rnrs)
    (ironscheme fsm-cond)
    (ironscheme contracts)
    (ironscheme clr))
    
  (clr-using System.Text.RegularExpressions)
  
  (define (regex? obj)
    (clr-is Regex obj))
    
  (define/contract (make-regex pattern:string)
    (clr-new Regex pattern 'compiled))   
    
  (define/contract (regex-match input:string pattern:string)
    (clr-static-call Regex Match input pattern))

  (define/contract (regex-matches input:string pattern:string)
    (clr-static-call IronScheme.Runtime.Cons FromList
      (clr-static-call Regex Matches input pattern)))

  (define (match? obj)
    (clr-is Match obj))      
      
  (define (group? obj)
    (clr-is Group obj))      
    
  (define/contract (group-value group:group)
    (and (group-success? group)
         (clr-prop-get Group Value group)))
    
  (define/contract (group-success? group:group)
    (clr-prop-get Group Success group))    

  (define/contract (match-group match:match group-name:string)
    (clr-indexer-get GroupCollection 
      (clr-prop-get Match Groups match) 
      (clr-cast String group-name)))
  
  (define/contract (regex-match? input:string pattern:string)
    (clr-static-call Regex IsMatch input pattern))

  (define/contract (regex-split input:string pattern/re)
    (if (regex? pattern/re)
        (clr-call Regex Split pattern/re input)
        (clr-static-call Regex Split input pattern/re)))
    
  (define/contract (regex-replace input:string pattern/re replacement/evaluator)
    (fsm-cond (pattern/re replacement/evaluator)
      [(regex?  string?)    (clr-call Regex (Replace String String) pattern/re input replacement/evaluator)]
      [(string? string?)    (clr-static-call Regex (Replace String String String) input pattern/re replacement/evaluator)]
      [(string? procedure?) (clr-static-call Regex (Replace String String MatchEvaluator) input pattern/re replacement/evaluator)]
      [(regex?  procedure?) (clr-call Regex (Replace String MatchEvaluator) pattern/re input replacement/evaluator)]
      [else 
        (assertion-violation 'regex-replace "not valid arguments" pattern/re replacement/evaluator)]))

  (define/contract (regex-escape input:string)
    (clr-static-call Regex Escape input))

  (define/contract (regex-unescape input:string)
    (clr-static-call Regex Unescape input)))
    