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

(library (ironscheme collections arraylist)
  (export
    make-arraylist
    arraylist?
    arraylist-lastindexof
    arraylist-addrange!
    arraylist-insertrange!
    arraylist-removerange!
    arraylist-sort!
    arraylist->vector
    arraylist-clone
    arraylist-reverse!
    (rename 
      (ilist-add! arraylist-add!)
      (ilist-insert! arraylist-insert!)
      (ilist-removeat! arraylist-removeat!)
      (ilist-remove! arraylist-remove!)
      (ilist-count arraylist-count)
      (ilist-clear! arraylist-clear!)
      (ilist-contains? arraylist-contains?)
      (ilist-indexof arraylist-indexof)
      (ilist-ref arraylist-ref)
      (ilist-set! arraylist-set!)))
  (import 
    (rnrs)
    (ironscheme clr)
    (ironscheme collections ilist))

  (clr-using System.Collections)
  
  (define make-arraylist
    (case-lambda
      [()     (clr-new ArrayList)]
      [(col)  (clr-new ArrayList (clr-cast ICollection col))]))
    
  (define (arraylist? o)
    (clr-is ArrayList o))
    
  (define (arraylist-lastindexof s o)
    (clr-call ArrayList LastIndexOf s o))    
    
  (define (arraylist-addrange! s o)
    (clr-call ArrayList AddRange s o))    

  (define (arraylist-insertrange! s n c)
    (clr-call ArrayList InsertRange s n c))

  (define (arraylist-removerange! s start c)
    (clr-call ArrayList RemoveRange s start c))    
  
  (define (arraylist-sort! s)
    (clr-call ArrayList Sort s))
    
  (define (arraylist->vector s)
    (clr-call ArrayList ToArray s))
    
  (define (arraylist-clone s)
    (clr-call ArrayList Clone s))
  
  (define (arraylist-reverse! s)
    (clr-call ArrayList Reverse s))

)