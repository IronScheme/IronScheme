(library (ironscheme threading)
  (export
    make-thread
    thread?
    queue-work-item
    start-thread)
    
  (import 
    (rnrs)
    (ironscheme clr))
    
  (clr-using system.threading)
  
  (define (start-thread thread)
    (clr-call thread start thread))
      
  (define (thread? obj)
    (clr-is thread obj))
    
  (define (make-thread proc)
    (clr-new thread proc))  
     
  (define queue-work-item
    (case-lambda 
      [(proc)       (queue-work-item proc #f)]
      [(proc state) (clr-static-call threadpool queueuserworkitem proc state)]))
      
  (clr-clear-usings)      
)