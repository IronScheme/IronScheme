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

  (clr-using system.collections)
  
  (define make-arraylist
    (case-lambda
      [()     (clr-new arraylist)]
      [(col)  (clr-new arraylist (clr-cast icollection col))]))
    
  (define (arraylist? o)
    (clr-is arraylist o))
    
  (define (arraylist-lastindexof s o)
    (clr-call arraylist lastindexof s o))    
    
  (define (arraylist-addrange! s o)
    (clr-call arraylist addrange s o))    

  (define (arraylist-insertrange! s n c)
    (clr-call arraylist insertrange s n c))

  (define (arraylist-removerange! s start c)
    (clr-call arraylist removerange s start c))    
  
  (define (arraylist-sort! s)
    (clr-call arraylist sort s))
    
  (define (arraylist->vector s)
    (clr-call arraylist toarray s))
    
  (define (arraylist-clone s)
    (clr-call arraylist clone s))
  
  (define (arraylist-reverse! s)
    (clr-call arraylist reverse s))

)