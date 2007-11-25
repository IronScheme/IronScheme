(import (rnrs))

(define ht (make-eqv-hashtable 10))

(hashtable-set! ht 1 "i")
(hashtable-set! ht 2 "r")
(hashtable-set! ht 3 "o")
(hashtable-set! ht 4 "n")

(assert (eqv? "i" (hashtable-ref ht 1 #f)))
(assert (eqv? "r" (hashtable-ref ht 2 #f)))
(assert (eqv? "o" (hashtable-ref ht 3 #f)))
(assert (eqv? "n" (hashtable-ref ht 4 #f)))
(assert (eqv?  #f (hashtable-ref ht 5 #f)))

(assert (hashtable-contains? ht 4))
(assert (not (hashtable-contains? ht 5)))

	
(assert (eqv? (hashtable-size ht) 4))

(hashtable-delete! ht 1)
(hashtable-delete! ht 2)
(hashtable-delete! ht 3)
(hashtable-delete! ht 4)

(assert (eqv? (hashtable-size ht) 0))
