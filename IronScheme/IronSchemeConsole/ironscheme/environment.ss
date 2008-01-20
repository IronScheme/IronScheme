(library (ironscheme environment)
  (export
    get-working-directory
    set-working-directory!
    get-current-user
    get-current-user-domain
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
  
  (define (get-working-directory)
    (clr-static-prop-get environment currentdirectory))
    
  (define (set-working-directory! path)
    (clr-static-prop-set! environment currentdirectory path))
    

  (define (get-current-user)
    (clr-static-prop-get environment username))

  (define (get-current-user-domain)
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