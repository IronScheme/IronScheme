#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

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
    (ironscheme contracts)
    (ironscheme clr))
    
  (clr-using System.Diagnostics)
  
  (define (process? obj)
    (clr-is Process obj))
    
  (define/contract (process-start p:process)
    (clr-static-call IronScheme.Runtime.Helpers StartProcess p))
    
  (define/contract wait-for-exit
    (case-lambda
      [(p:process)     (clr-call Process WaitForExit p)]
      [(p:process ms)  (clr-call Process WaitForExit p ms)]))
      
  (define/contract wait-for-input-idle
    (case-lambda 
      [(p:process)     (clr-call Process WaitForInputIdle p)]
      [(p:process ms)  (clr-call Process WaitForInputIdle p ms)]))      
      
  (define/contract get-processes
    (case-lambda 
      [()      
        (clr-static-call Process GetProcesses)]
      [(host:string)  
        (clr-static-call Process GetProcesses host)]))
      
  (define/contract get-processes-by-name
    (case-lambda 
      [(name:string)      
        (clr-static-call Process GetProcessesByName name)]
      [(name:string host:string) 
        (clr-static-call Process GetProcessesByName name host)]))
      
  (define/contract get-process-by-id
    (case-lambda 
      [(id)      
        (clr-static-call Process GetProcessById id)]
      [(id host:string) 
        (clr-static-call Process GetProcessById id host)]))  

  (define (get-current-process)
    (clr-static-call Process GetCurrentProcess))
    
  (define/contract (process-kill p:process)
    (clr-call Process Kill p))
    
  (define/contract (process-refresh p:process)
    (clr-call Process Refresh p))
    
  (define/contract (process-input-port p:process)
    (clr-prop-get Process StandardInput p))

  (define/contract (process-output-port p:process)
    (clr-prop-get Process StandardOutput p))

  (define/contract (process-error-port p:process)
    (clr-prop-get Process StandardError p))
    
  (define/contract (process-responding? p:process)
    (clr-prop-get Process Responding p))    
    
  (define/contract (process-exited? p:process)
    (clr-prop-get Process HasExited p))    
    
  (define/contract (process-exit-code p:process)
    (clr-prop-get Process ExitCode p))    
    
  (define/contract (process-name p:process)
    (clr-prop-get Process ProcessName p))    

  (define/contract (process-id p:process)
    (clr-prop-get Process Id p))    
    
  (define/contract (process-start-time p:process)
    (clr-prop-get Process StartTime p))    

  (define/contract (process-exit-time p:process)
    (clr-prop-get Process ExitTime p))    
    
  (define/contract (process-user-cpu-time p:process)
    (clr-prop-get Process UserProcessorTime p))     

  (define/contract (process-total-cpu-time p:process)
    (clr-prop-get Process TotalProcessorTime p))     

  (define/contract (process-system-cpu-time p:process)
    (clr-prop-get Process PrivilegedProcessorTime p))      
    
  (define/contract (process-private-memory-size p:process)
    (clr-prop-get Process PrivateMemorySize p))             
    
  (define/contract (process-virtual-memory-size p:process)
    (clr-prop-get Process VirtualMemorySize p))       
    
  (define/contract (process-working-set p:process)
    (clr-prop-get Process WorkingSet p))       
    
  (define/contract (process-peak-working-set p:process)
    (clr-prop-get Process PeakWorkingSet p))      

  (define/contract (process-paged-system-memory-size p:process)
    (clr-prop-get Process PagedSystemMemorySize p)) 

  (define/contract (process-paged-memory-size p:process)
    (clr-prop-get Process PagedMemorySize p))      

  (define/contract (process-non-paged-system-memory-size p:process)
    (clr-prop-get Process NonpagedSystemMemorySize p))      

  (define/contract (process-peak-paged-memory-size p:process)
    (clr-prop-get Process PeakPagedMemorySize p))      

  (define/contract (process-peak-virtual-memory-size p:process)
    (clr-prop-get Process PeakVirtualMemorySize p))         

  (define/contract process-priority
    (case-lambda
      [(p:process)            (clr-prop-get Process PriorityClass p)]
      [(p:process priority)   (clr-prop-set! Process PriorityClass p priority)]))

  (define/contract process-boost
    (case-lambda
      [(p:process)            (clr-prop-get Process PriorityBoostEnabled p)]
      [(p:process value)      (clr-prop-set! Process PriorityBoostEnabled p value)]))
  
  (define/contract make-process
    (case-lambda 
      [(filename:string args:string)              
        (clr-static-call IronScheme.Runtime.Helpers MakeProcess filename args #t #f #f #f)]
      [(filename:string args:string show?:boolean)        
        (clr-static-call IronScheme.Runtime.Helpers MakeProcess filename args show? #f #f #f)]
      [(filename:string args:string show?:boolean exit:procedure)   
        (clr-static-call IronScheme.Runtime.Helpers MakeProcess filename args show? exit #f #f)]
      [(filename:string args:string show?:boolean exit:procedure out err)   
        (clr-static-call IronScheme.Runtime.Helpers MakeProcess filename args show? exit out err)]))
     
)