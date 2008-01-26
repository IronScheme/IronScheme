(library (ironscheme environment)
  (export
    application-directory
    current-directory
    current-user
    current-user-domain
    get-hostname
    get-logical-drives
    get-environment-variables
    get-environment-variable
    set-environment-variable!
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
    
  (define (get-hostname)
    (clr-static-prop-get environment machinename))
    
  (define (get-logical-drives)
    (clr-static-call environment getlogicaldrives))    

  (define (get-environment-variables)
    (clr-static-call environment getenvironmentvariables))    ; pray this returns a HT

  (define (get-environment-variable name)
    (clr-static-call environment getenvironmentvariable name))    

  (define (set-environment-variable! name value)
    (clr-static-call environment setenvironmentvariable name value))    

    
    
  
  
  
  (clr-clear-usings)
)