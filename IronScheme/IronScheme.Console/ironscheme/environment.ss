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
    (ironscheme clr))

  (clr-using system)
  
  (define (application-directory)
    (clr-static-prop-get ironscheme.runtime.builtins applicationdirectory))
  
  (define current-directory
    (case-lambda
      [()       (clr-static-prop-get environment currentdirectory)]
      [(path)   (clr-static-prop-set! environment currentdirectory path)]))
    
  (define (current-user)
    (clr-static-prop-get environment username))

  (define (current-user-domain)
    (clr-static-prop-get environment userdomainname))
    
  (define (get-netbiosname)
    (clr-static-prop-get environment machinename))
    
  (define (get-hostname)
    (clr-static-call system.net.dns gethostname))
    
  (define (get-logical-drives)
    (clr-static-call environment getlogicaldrives))    

  (define (get-environment-variables)
    (clr-static-call environment getenvironmentvariables))    ; returns a hashtable

  (define (get-environment-variable name)
    (let ((v (clr-static-call environment getenvironmentvariable name)))
      (if (null? v) #f v)))

  (define (set-environment-variable! name value)
    (clr-static-call environment setenvironmentvariable name value))    
    
  (define (expand-environment-variables name)
    (clr-static-call environment expandenvironmentvariables name))    

)