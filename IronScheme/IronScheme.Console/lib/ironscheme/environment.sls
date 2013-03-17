#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

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
    expand-environment-variables)
  (import 
    (rnrs)
    (ironscheme contracts)
    (ironscheme clr))
  
  (define (application-directory)
    (clr-static-prop-get IronScheme.Runtime.Builtins ApplicationDirectory))
  
  (define/contract current-directory
    (case-lambda
      [()       
        (clr-static-prop-get Environment CurrentDirectory)]
      [(path:string)   
        (clr-static-prop-set! Environment CurrentDirectory path)]))
    
  (define (current-user)
    (clr-static-prop-get Environment UserName))

  (define (current-user-domain)
    (clr-static-prop-get Environment UserDomainName))
    
  (define (get-netbiosname)
    (clr-static-prop-get Environment MachineName))
    
  (define (get-hostname)
    (clr-static-call System.Net.Dns GetHostName))
    
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
    (clr-static-call Environment ExpandEnvironmentVariables name)))