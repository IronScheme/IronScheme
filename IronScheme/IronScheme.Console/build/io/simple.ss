#| License
Copyright (c) 2007,2008,2009,2010 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

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
    
    close-input-port
    close-output-port
    
    read-char
    peek-char
    read
    
    write-char
    newline
    
    display
    write
    
    textual-input-port?
    textual-output-port?
    )
    
  (import 
    (ironscheme contracts)
    (except (ironscheme)
        peek-char 
        write-char
        read-char
        newline
        with-input-from-file
        with-output-to-file
        call-with-input-file
        call-with-output-file
        textual-input-port?
        textual-output-port?))

  (define (textual-input-port? obj)
    (and (input-port? obj)
         (textual-port? obj)))   
         
  (define (textual-output-port? obj)
    (and (output-port? obj)
         (textual-port? obj)))               
        
  (define/contract peek-char
    (case-lambda
      [()
        (peek-char (current-input-port))]
      [(port:textual-input-port)
        (lookahead-char port)]))
        
  (define/contract read-char
    (case-lambda
      [()       
        (read-char (current-input-port))]
      [(port:textual-input-port)   
        (get-char port)]))        
      
  (define/contract write-char
    (case-lambda
      [(chr)       
        (write-char chr (current-output-port))]
      [(chr:char port:textual-output-port)  
        (put-char port chr)]))        
         
  (define-syntax try
    (syntax-rules (finally)
      [(_ expr finally fin)
        (dynamic-wind
          (lambda () #f)
          (lambda () expr)
          (lambda () fin))]))  
          
  (define/contract (with-input-from-file filename:string thunk:procedure)
    (parameterize ((current-input-port (open-input-file filename)))
      (try (thunk)
        finally (close-input-port (current-input-port)))))

  (define/contract (with-output-to-file filename:string thunk:procedure)
    (parameterize ((current-output-port (open-output-file filename)))
      (try (thunk)
        finally (close-output-port (current-output-port)))))
        
  (define/contract (call-with-input-file filename:string proc:procedure)
    (let ((p (open-input-file filename)))
      (try (proc p)
        finally (close-input-port p))))   

  (define/contract (call-with-output-file filename:string proc:procedure)
    (let ((p (open-output-file filename)))
      (try (proc p)
        finally (close-output-port p))))     
        
  (define/contract newline
    (case-lambda
      [()       
        (newline (current-output-port))]
      [(port:textual-output-port)   
        (display "\n" port)]))                
)
