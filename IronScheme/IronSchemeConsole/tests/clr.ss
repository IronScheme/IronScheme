(import 
  (rnrs)
	(system)
  (system collections)) 

(define k 6)

(define ht (hashtable:new k))

(hashtable:item ht 1 "hello")

(display (hashtable:item ht 1))
(newline)

(display (console:out))
(newline)

(display (console:error))
(newline)

(console:write "hello {0} {1} {2}" "world" "from" "IronScheme")

(error 'me 'bad 'me)