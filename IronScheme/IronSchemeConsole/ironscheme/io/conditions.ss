
(library (ironscheme io conditions)
  (export
    
    &i/o
    make-i/o-error
    i/o-error?
    
    &i/o-read
    make-i/o-read-error
    i/o-read-error?
    
    &i/o-write
    make-i/o-write-error
    i/o-write-error?
    
    &i/o-invalid-position
    make-i/o-invalid-position-error
    i/o-invalid-position-error?
    i/o-error-position
    
    &i/o-filename
    make-i/o-filename-error
    i/o-filename-error?
    i/o-error-filename
    
    &i/o-file-protection
    make-i/o-file-protection-error
    i/o-file-protection-error?
    
    &i/o-file-is-read-only
    make-i/o-file-is-read-only-error
    i/o-file-is-read-only-error?
    
    &i/o-file-already-exists
    make-i/o-file-already-exists-error
    i/o-file-already-exists-error?
    
    &i/o-file-does-not-exist
    make-i/o-file-does-not-exist-error
    i/o-file-does-not-exist-error?
    
    &i/o-port
    make-i/o-port-error
    i/o-port-error?
    i/o-error-port
    
    &i/o-decoding 
    make-i/o-decoding-error 
    i/o-decoding-error?
    
    &i/o-encoding
    make-i/o-encoding-error 
    i/o-encoding-error?
    i/o-encoding-error-char
    )
    
  (import 
    (except (rnrs)
      &i/o
      make-i/o-error
      i/o-error?
      
      &i/o-read
      make-i/o-read-error
      i/o-read-error?
      
      &i/o-write
      make-i/o-write-error
      i/o-write-error?
      
      &i/o-invalid-position
      make-i/o-invalid-position-error
      i/o-invalid-position-error?
      i/o-error-position
      
      &i/o-filename
      make-i/o-filename-error
      i/o-filename-error?
      i/o-error-filename
      
      &i/o-file-protection
      make-i/o-file-protection-error
      i/o-file-protection-error?
      
      &i/o-file-is-read-only
      make-i/o-file-is-read-only-error
      i/o-file-is-read-only-error?
      
      &i/o-file-already-exists
      make-i/o-file-already-exists-error
      i/o-file-already-exists-error?
      
      &i/o-file-does-not-exist
      make-i/o-file-does-not-exist-error
      i/o-file-does-not-exist-error?
      
      &i/o-port
      make-i/o-port-error
      i/o-port-error?
      i/o-error-port
          
      &i/o-decoding 
      make-i/o-decoding-error 
      i/o-decoding-error?
      
      &i/o-encoding
      make-i/o-encoding-error 
      i/o-encoding-error?
      i/o-encoding-error-char))
  
  (define-condition-type &i/o &error 
    make-i/o-error i/o-error?)

  (define-condition-type &i/o-read &i/o
    make-i/o-read-error i/o-read-error?)

  (define-condition-type &i/o-write &i/o
    make-i/o-write-error i/o-write-error?)

  (define-condition-type &i/o-invalid-position &i/o
    make-i/o-invalid-position-error i/o-invalid-position-error?
    (position i/o-error-position))

  (define-condition-type &i/o-filename &i/o
    make-i/o-filename-error i/o-filename-error?
    (filename i/o-error-filename))

  (define-condition-type &i/o-file-protection &i/o-filename
    make-i/o-file-protection-error i/o-file-protection-error?)

  (define-condition-type &i/o-file-is-read-only &i/o-file-protection
    make-i/o-file-is-read-only-error i/o-file-is-read-only-error?)

  (define-condition-type &i/o-file-already-exists &i/o-filename
    make-i/o-file-already-exists-error i/o-file-already-exists-error?)

  (define-condition-type &i/o-file-does-not-exist &i/o-filename
    make-i/o-file-does-not-exist-error i/o-file-does-not-exist-error?)

  (define-condition-type &i/o-port &i/o
    make-i/o-port-error i/o-port-error?
    (port i/o-error-port))

  (define-condition-type &i/o-decoding &i/o-port
    make-i/o-decoding-error i/o-decoding-error?)

  (define-condition-type &i/o-encoding &i/o-port
    make-i/o-encoding-error i/o-encoding-error?
    (char i/o-encoding-error-char))
)