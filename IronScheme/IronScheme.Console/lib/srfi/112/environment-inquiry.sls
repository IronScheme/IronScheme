(library (srfi :112 environment-inquiry)
  (export 
    implementation-name
    implementation-version
    cpu-architecture
    machine-name
    os-name
    os-version)
  (import 
    (ironscheme)
    (ironscheme clr)
    (ironscheme environment))
    
  (define (implementation-name) "IronScheme")
  
  (define (implementation-version) 
    (ironscheme-version))
    
  (define (cpu-architecture)
    (if (= (clr-static-prop-get IntPtr Size) 4)
        "x86"
        "amd64"))
    
  (define (machine-name)
    (get-netbiosname))
    
  (define os-info (clr-static-prop-get Environment OSVersion))    
    
  (define (os-name) 
    (clr-call Object ToString 
      (clr-prop-get OperatingSystem Platform os-info)))
  
  (define (os-version) 
    (clr-call Object ToString 
      (clr-prop-get OperatingSystem Version os-info))))