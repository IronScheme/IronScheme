(library (ironscheme io simple)
  (export
    eof-object
    eof-object?
    
    call-with-input-file
    call-with-output-file
    
    input-port?
    output-port?
    
    current-input-port
    current-output-port
    current-error-port
    
    with-input-from-file
    with-output-to-file
    
    open-input-file
    open-output-file
    
    close-input-port
    close-output-port
    
    read-char
    peek-char
    read
    
    write-char
    newline
    
    display
    write)
    
  (import 
    (except (ironscheme)
        peek-char 
        write-char
        read-char
        newline
        with-input-from-file
        with-output-to-file
        call-with-input-file
        call-with-output-file))
        
  ;(define (printer obj port caller)
    ;)
        ;
  ;(define display
    ;(case-lambda
      ;[(obj)
        ;(display obj (current-output-port))]
      ;[(obj port)
        ;(        
        
  (define peek-char
    (case-lambda
      [()       (peek-char (current-input-port))]
      [(port)   (lookahead-char port)]))            
        
  (define read-char
    (case-lambda
      [()       (read-char (current-input-port))]
      [(port)   (get-char port)]))        
      
  (define write-char
    (case-lambda
      [(chr)       (write-char chr (current-output-port))]
      [(chr port)  (put-char port chr)]))        
      
         
  (define-syntax try
    (syntax-rules (finally)
      [(_ expr finally fin)
        (dynamic-wind
          (lambda () #f)
          (lambda () expr)
          (lambda () fin))]))        
         
  (define (with-input-from-file filename thunk)
    (parameterize ((current-input-port (open-input-file filename)))
      (try (thunk)
        finally (close-input-port (current-input-port)))))

  (define (with-output-to-file filename thunk)
    (parameterize ((current-output-port (open-output-file filename)))
      (try (thunk)
        finally (close-output-port (current-output-port)))))
        
  (define (call-with-input-file filename proc)
    (let ((p (open-input-file filename)))
      (try (proc p)
        finally (close-input-port p))))   

  (define (call-with-output-file filename proc)
    (let ((p (open-output-file filename)))
      (try (proc p)
        finally (close-output-port p))))     
        
  (define newline
    (case-lambda
      [()       (newline (current-output-port))]
      [(port)   (display "\n" port)]))                
)
