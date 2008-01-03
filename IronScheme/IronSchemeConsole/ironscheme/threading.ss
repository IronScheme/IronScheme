(library (ironscheme threading)
  (export
    make-thread
    thread?
    queue-work-item
    start-thread)
    
  (import 
    (rnrs)
    (ironscheme clr))
  
  (define (start-thread thread)
    (clr-call system.threading.thread start thread))
      
  (define (thread? obj)
    (clr-is system.threading.thread obj))
    
  (define (make-thread proc)
    (clr-new system.threading.thread (clr-cast system.threading.threadstart proc)))  
     
  (define queue-work-item
    (case-lambda 
      [(proc)       (queue-work-item proc #f)]
      [(proc state) (clr-static-call system.threading.threadpool queueuserworkitem (clr-cast system.threading.waitcallback proc) state)]))
)