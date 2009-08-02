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
    (clr-static-call Ironscheme.Runtime.Helpers StartProcess p))
    
  (define wait-for-exit
    (case/contract 
      [(p:process)     (clr-call Process waitforexit p)]
      [(p:process ms)  (clr-call Process waitforexit p ms)]))
      
  (define wait-for-input-idle
    (case/contract 
      [(p:process)     (clr-call Process waitforinputidle p)]
      [(p:process ms)  (clr-call Process waitforinputidle p ms)]))      
      
  (define get-processes
    (case/contract 
      [()      
        (clr-static-call Process getprocesses)]
      [(host:string)  
        (clr-static-call Process getprocesses host)]))
      
  (define get-processes-by-name
    (case/contract 
      [(name:string)      
        (clr-static-call Process getprocessesbyname name)]
      [(name:string host:string) 
        (clr-static-call Process getprocessesbyname name host)]))
      
  (define get-process-by-id
    (case/contract 
      [(id)      
        (clr-static-call Process getprocessbyid id)]
      [(id host:string) 
        (clr-static-call Process getprocessbyid id host)]))  

  (define (get-current-process)
    (clr-static-call Process getcurrentprocess))
    
  (define/contract (process-kill p:process)
    (clr-call Process kill p))
    
  (define/contract (process-refresh p:process)
    (clr-call Process refresh p))
    
  (define/contract (process-input-port p:process)
    (clr-prop-get Process standardinput p))

  (define/contract (process-output-port p:process)
    (clr-prop-get Process standardoutput p))

  (define/contract (process-error-port p:process)
    (clr-prop-get Process standarderror p))
    
  (define/contract (process-responding? p:process)
    (clr-prop-get Process responding p))    
    
  (define/contract (process-exited? p:process)
    (clr-prop-get Process hasexited p))    
    
  (define/contract (process-exit-code p:process)
    (clr-prop-get Process exitcode p))    
    
  (define/contract (process-name p:process)
    (clr-prop-get Process processname p))    

  (define/contract (process-id p:process)
    (clr-prop-get Process id p))    
    
  (define/contract (process-start-time p:process)
    (clr-prop-get Process starttime p))    

  (define/contract (process-exit-time p:process)
    (clr-prop-get Process exittime p))    
    
  (define/contract (process-user-cpu-time p:process)
    (clr-prop-get Process userprocessortime p))     

  (define/contract (process-total-cpu-time p:process)
    (clr-prop-get Process totalprocessortime p))     

  (define/contract (process-system-cpu-time p:process)
    (clr-prop-get Process privilegedprocessortime p))      
    
  (define/contract (process-private-memory-size p:process)
    (clr-prop-get Process privatememorysize p))             
    
  (define/contract (process-virtual-memory-size p:process)
    (clr-prop-get Process virtualmemorysize p))       
    
  (define/contract (process-working-set p:process)
    (clr-prop-get Process workingset p))       
    
  (define/contract (process-peak-working-set p:process)
    (clr-prop-get Process peakworkingset p))      

  (define/contract (process-paged-system-memory-size p:process)
    (clr-prop-get Process pagedsystemmemorysize p)) 

  (define/contract (process-paged-memory-size p:process)
    (clr-prop-get Process pagedmemorysize p))      

  (define/contract (process-non-paged-system-memory-size p:process)
    (clr-prop-get Process nonpagedsystemmemorysize p))      

  (define/contract (process-peak-paged-memory-size p:process)
    (clr-prop-get Process peakpagedmemorysize p))      

  (define/contract (process-peak-virtual-memory-size p:process)
    (clr-prop-get Process peakvirtualmemorysize p))         

  (define process-priority
    (case/contract
      [(p:process)            (clr-prop-get Process priorityclass p)]
      [(p:process priority)   (clr-prop-set! Process priorityclass p priority)]))

  (define process-boost
    (case/contract
      [(p:process)            (clr-prop-get Process priorityboostenabled p)]
      [(p:process value)      (clr-prop-set! Process priorityboostenabled p value)]))
  
  (define make-process
    (case/contract 
      [(filename:string args:string)              
        (clr-static-call ironscheme.runtime.helpers makeprocess filename args #t #f #f #f)]
      [(filename:string args:string show?:boolean)        
        (clr-static-call ironscheme.runtime.helpers makeprocess filename args show? #f #f #f)]
      [(filename:string args:string show?:boolean exit:procedure)   
        (clr-static-call ironscheme.runtime.helpers makeprocess filename args show? exit #f #f)]
      [(filename:string args:string show?:boolean exit:procedure out err)   
        (clr-static-call ironscheme.runtime.helpers makeprocess filename args show? exit out err)]))
     
)