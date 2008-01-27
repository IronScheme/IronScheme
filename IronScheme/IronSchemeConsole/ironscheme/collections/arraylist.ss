(library (ironscheme collections arraylist)
  (export
    make-arraylist
    arraylist?
    arraylist-clear!
    arraylist-contains?
    arraylist-indexof
    arraylist-lastindexof
    arraylist-addrange!
    arraylist-add!
    arraylist-insert!
    arraylist-insertrange!
    arraylist-removeat!
    arraylist-remove!
    arraylist-removerange!
    arraylist-sort!
    arraylist->vector
    arraylist-count
    arraylist-clone
    arraylist-reverse!)
  (import 
    (rnrs)
    (ironscheme clr))

  (clr-using system.collections)
  
  (define make-arraylist
    (case-lambda
      [()     (clr-new arraylist)]
      [(col)  (clr-new arraylist (clr-cast icollection col))]))
    
  (define (arraylist? o)
    (clr-is arraylist o))
  
  (define (arraylist-clear! s)
    (clr-call arraylist clear s))
  
  (define (arraylist-contains? s o)
    (clr-call arraylist contains s o))

  (define (arraylist-indexof s o)
    (clr-call arraylist indexof s o))
    
  (define (arraylist-lastindexof s o)
    (clr-call arraylist lastindexof s o))    
    
  (define (arraylist-addrange! s o)
    (clr-call arraylist addrange s o))    

  (define (arraylist-insertrange! s n c)
    (clr-call arraylist insertrange s n c))

  (define (arraylist-insert! s n o)
    (clr-call arraylist insert s n o))

  (define (arraylist-add! s o)
    (clr-call arraylist add s o))
    
  (define (arraylist-removerange! s start c)
    (clr-call arraylist removerange s start c))    
  
  (define (arraylist-remove! s o)
    (clr-call arraylist remove s o))
    
  (define (arraylist-removeat! s n)
    (clr-call arraylist removeat s n))    
  
  (define (arraylist-sort! s)
    (clr-call arraylist sort s))
    
  (define (arraylist->vector s)
    (clr-call arraylist toarray s))
    
  (define (arraylist-clone s)
    (clr-call arraylist clone s))
    
  (define (arraylist-count s)
    (clr-prop-get arraylist count s))
  
  (define (arraylist-reverse! s)
    (clr-call arraylist reverse s))
    
  (clr-clear-usings)
)