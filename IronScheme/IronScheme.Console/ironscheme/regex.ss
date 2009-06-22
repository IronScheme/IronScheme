#| ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 2007,2008,2009
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************|#

(library (ironscheme regex)
  (export
    regex?
    make-regex
    match-value
    match-group
    match-success?
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
    
  (define (make-regex pattern)
    (clr-new regex pattern 'compiled))   
    
  (define (regex-match input pattern)
    (clr-static-call regex match input pattern))

  (define (regex-matches input pattern)
    (clr-static-call IronScheme.Runtime.Cons FromList
      (clr-static-call regex matches input pattern)))
    
  (define (match-value match)
    (and 
      (match-success? match)
      (clr-prop-get group value match)))
    
  (define (match-success? match)
    (clr-prop-get group success match))    

  (define (match-group match group-name)
    (clr-indexer-get groupcollection 
      (clr-prop-get match groups match) 
      (clr-cast system.string group-name)))
  
  (define (regex-match? input pattern)
    (clr-static-call regex ismatch input pattern))

  (define (regex-split input pattern/re)
    (if (regex? pattern/re)
      (clr-call regex split pattern/re input)
      (clr-static-call regex split input pattern/re)))
    
  (define (regex-replace input pattern replacement)
    (clr-static-call regex replace input pattern (clr-cast system.string replacement))) ; need cast to deal with overload

  (define (regex-escape input)
    (clr-static-call regex escape input))

  (define (regex-unescape input)
    (clr-static-call regex unescape input))
 
)
    