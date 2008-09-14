(library (ironscheme io ports)
  (export
    file-options
    
    buffer-mode?
    
    latin-1-codec
    utf-8-codec
    utf-16-codec
    eol-style
    native-eol-style
    
    &i/o-decoding
    make-i/o-decoding-error
    i/o-decoding-error?
    &i/o-encoding
    make-i/o-encoding-error
    i/o-encoding-error?
    i/o-encoding-error-char
    
    error-handling-mode
    make-transcoder
    native-transcoder
    transcoder-codec
    transcoder-eol-style
    transcoder-error-handling-mode
    
    bytevector->string
    string->bytevector
    
    eof-object
    eof-object?
    
    port?
    port-transcoder
    textual-port?
    binary-port?
    transcoded-port
    
    port-has-port-position?
    port-position
    port-has-set-port-position!?
    set-port-position!
    
    close-port
    call-with-port
    
    input-port?
    port-eof?
    open-file-input-port
    open-bytevector-input-port
    open-string-input-port
    standard-input-port
    current-input-port
    make-custom-binary-input-port
    make-custom-textual-input-port
    
    get-u8
    lookahead-u8
    get-bytevector-n
    get-bytevector-n!
    get-bytevector-some
    get-bytevector-all
    
    get-char
    lookahead-char
    get-string-n
    get-string-n!
    get-string-all
    get-line
    get-datum
    
    output-port?
    flush-output-port
    output-port-buffer-mode
    open-file-output-port
    open-bytevector-output-port
    call-with-bytevector-output-port
    open-string-output-port
    call-with-string-output-port
    
    standard-output-port
    standard-error-port
    
    current-output-port
    current-error-port
    
    make-custom-binary-output-port
    make-custom-textual-output-port
    
    put-u8
    put-bytevector
    
    put-char
    put-string
    put-datum
    
    open-file-input/output-port
    
    make-custom-binary-input/output-port
    make-custom-textual-input/output-port
    )
  
  (import 
    (ironscheme clr)
    (except (rnrs) open-string-output-port port?))
    
  (define (get-output-string port)
    (clr-call ironscheme.runtime.stringwriter getbuffer port))
    
  (define (open-output-string)
    (clr-new ironscheme.runtime.stringwriter))
  
  (define (open-string-output-port)
    (let ((p (open-output-string)))
      (values p (lambda () (get-output-string p)))))
      
  (define (port? obj)
    (or (textual-port? obj) (binary-port? obj)))
)
