(library (ironscheme process)
  (export
    wait-for-exit
    wait-for-input-idle
    make-process
    process-start
    get-processes
    get-processes-by-name
    get-process-by-id
    get-current-process
    process-kill
    process-input-port
    process-output-port
    process-error-port
    process-refresh
    process-responding?
    process-start-time
    process-exit-time
    process-name
    process-exited?
    process-exit-code
    process-id
    process-priority
    process-boost
    process-user-cpu-time
    process-total-cpu-time
    process-system-cpu-time
    process-private-memory-size
    process-virtual-memory-size
    process-working-set
    process-peak-working-set
    process-paged-system-memory-size
    process-paged-memory-size
    process-non-paged-system-memory-size
    process-peak-paged-memory-size
    process-peak-virtual-memory-size
    process?)
    
    
  (import 
    (rnrs)
    (ironscheme clr))
    
  (clr-using system.diagnostics)
  
  (define (process? obj)
    (clr-is process obj))
    
  (define (process-start p)
    (clr-static-call ironscheme.runtime.helpers startprocess p))
    
  (define wait-for-exit
    (case-lambda
      [(p)     (clr-call process waitforexit p)]
      [(p ms)  (clr-call process waitforexit p ms)]))
      
  (define wait-for-input-idle
    (case-lambda
      [(p)     (clr-call process waitforinputidle p)]
      [(p ms)  (clr-call process waitforinputidle p ms)]))      
      
  (define get-processes
    (case-lambda
      [()      (clr-static-call process getprocesses)]
      [(host)  (clr-static-call process getprocesses host)]))
      
  (define get-processes-by-name
    (case-lambda
      [(name)      (clr-static-call process getprocessesbyname name)]
      [(name host) (clr-static-call process getprocessesbyname name host)]))
      
  (define get-process-by-id
    (case-lambda
      [(id)      (clr-static-call process getprocessbyid id)]
      [(id host) (clr-static-call process getprocessbyid id host)]))  

  (define (get-current-process)
    (clr-static-call process getcurrentprocess))
    
  (define (process-kill p)
    (clr-call process kill p))
    
  (define (process-refresh p)
    (clr-call process refresh p))
    
  (define (process-input-port p)
    (clr-prop-get process standardinput p))

  (define (process-output-port p)
    (clr-prop-get process standardoutput p))

  (define (process-error-port p)
    (clr-prop-get process standarderror p))
    
  (define (process-responding? p)
    (clr-prop-get process responding p))    
    
  (define (process-exited? p)
    (clr-prop-get process hasexited p))    
    
  (define (process-exit-code p)
    (clr-prop-get process exitcode p))    
    
  (define (process-name p)
    (clr-prop-get process processname p))    

  (define (process-id p)
    (clr-prop-get process id p))    
    
  (define (process-start-time p)
    (clr-prop-get process starttime p))    

  (define (process-exit-time p)
    (clr-prop-get process exittime p))    
    
  (define (process-user-cpu-time p)
    (clr-prop-get process userprocessortime p))     

  (define (process-total-cpu-time p)
    (clr-prop-get process totalprocessortime p))     

  (define (process-system-cpu-time p)
    (clr-prop-get process privilegedprocessortime p))      
    
  (define (process-private-memory-size p)
    (clr-prop-get process privatememorysize p))             
    
  (define (process-virtual-memory-size p)
    (clr-prop-get process virtualmemorysize p))       
    
  (define (process-working-set p)
    (clr-prop-get process workingset p))       
    
  (define (process-peak-working-set p)
    (clr-prop-get process peakworkingset p))      

  (define (process-paged-system-memory-size p)
    (clr-prop-get process pagedsystemmemorysize p)) 

  (define (process-paged-memory-size p)
    (clr-prop-get process pagedmemorysize p))      

  (define (process-non-paged-system-memory-size p)
    (clr-prop-get process nonpagedsystemmemorysize p))      

  (define (process-peak-paged-memory-size p)
    (clr-prop-get process peakpagedmemorysize p))      

  (define (process-peak-virtual-memory-size p)
    (clr-prop-get process peakvirtualmemorysize p))         
    
    
  (define process-priority
    (case-lambda
      [(p)            (clr-prop-get process priorityclass p)]
      [(p priority)   (clr-prop-set! process priorityclass p priority)]))

  (define process-boost
    (case-lambda
      [(p)            (clr-prop-get process priorityboostenabled p)]
      [(p value)      (clr-prop-set! process priorityboostenabled p value)]))
  
  (define make-process
    (case-lambda 
      [(filename args)              (clr-static-call ironscheme.runtime.helpers makeprocess filename args #t #f #f #f)]
      [(filename args show?)        (clr-static-call ironscheme.runtime.helpers makeprocess filename args show? #f #f #f)]
      [(filename args show? exit)   (clr-static-call ironscheme.runtime.helpers makeprocess filename args show? exit #f #f)]
      [(filename args show? exit out err)   (clr-static-call ironscheme.runtime.helpers makeprocess filename args show? exit out err)]))
     
)