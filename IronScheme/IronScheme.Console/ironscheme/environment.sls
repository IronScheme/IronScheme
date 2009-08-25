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

(library (ironscheme environment)
  (export
    application-directory
    current-directory
    current-user
    current-user-domain
    get-hostname
    get-netbiosname
    get-logical-drives
    get-environment-variables
    get-environment-variable
    set-environment-variable!
    expand-environment-variables
    )
  (import 
    (rnrs)
    (ironscheme contracts)
    (ironscheme clr))

  (clr-using System)
  
  (define (application-directory)
    (clr-static-prop-get Ironscheme.Runtime.Builtins ApplicationDirectory))
  
  (define/contract current-directory
    (case-lambda
      [()       
        (clr-static-prop-get Environment CurrentDirectory)]
      [(path:string)   
        (clr-static-prop-set! Environment CurrentDirectory path)]))
    
  (define (current-user)
    (clr-static-prop-get Environment Username))

  (define (current-user-domain)
    (clr-static-prop-get Environment UserDomainname))
    
  (define (get-netbiosname)
    (clr-static-prop-get Environment Machinename))
    
  (define (get-hostname)
    (clr-static-call System.Net.Dns GetHostname))
    
  (define (get-logical-drives)
    (clr-static-call Environment GetLogicalDrives))    

  (define (get-environment-variables)
    (clr-static-call Environment GetEnvironmentVariables))    ; returns a hashtable

  (define/contract (get-environment-variable name:string)
    (let ((v (clr-static-call Environment GetEnvironmentVariable name)))
      (if (null? v) #f v)))

  (define/contract (set-environment-variable! name:string value:string)
    (clr-static-call Environment SetEnvironmentVariable name value))    
    
  (define/contract (expand-environment-variables name:string)
    (clr-static-call Environment ExpandEnvironmentVariables name))    

)