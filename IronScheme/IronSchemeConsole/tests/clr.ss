(import 
  (rnrs)
  (system collections)) 

(define ht (hashtable:new))

(hashtable:item ht 1 "hello")

(display (hashtable:item ht 1))
(newline)

