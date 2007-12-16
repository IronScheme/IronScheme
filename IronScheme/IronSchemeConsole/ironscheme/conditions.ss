
(library (ironscheme conditions (6))
  (export
    
    &condition
    condition
    simple-conditions
    condition?
    condition-predicate
    condition-accessor
    define-condition-type
  
    &message
    make-message-condition
    message-condition?
    condition-message
    
    &warning
    make-warning
    warning?
    
    &serious
    make-serious-condition
    serious-condition?
    
    &error
    make-error
    error?
    
    &violation
    make-violation
    violation?
    
    &assertion
    make-assertion-violation
    assertion-violation?
    
    &irritants
    make-irritants-condition
    irritants-condition?
    condition-irritants
    
    &who
    make-who-condition
    who-condition?
    condition-who
    
    &non-continuable
    make-non-continuable-violation
    non-continuable-violation?
    
    &implementation-restriction
    make-implementation-restriction-violation
    implementation-restriction-violation?
    
    &lexical
    make-lexical-violation
    lexical-violation?
    
    &syntax
    make-syntax-violation
    syntax-violation?
    syntax-violation-form
    syntax-violation-subform
    
    &undefined
    make-undefined-violation
    undefined-violation?    
 )
  (import 
    (rnrs base)
    (rnrs records syntactic)
    (only (rnrs conditions)
      condition
      simple-conditions
      condition?
      condition-predicate
      condition-accessor 
      define-condition-type))
  
  (define-record-type &condition (nongenerative))
  
  (define &condition-rtd (record-type-descriptor &condition))
  (define &condition-rcd (record-constructor-descriptor &condition)) 
  
  (define-condition-type &message &condition
    make-message-condition message-condition? 
    (message condition-message))

  (define-condition-type &warning &condition
    make-warning warning?)

  (define-condition-type &serious &condition 
    make-serious-condition serious-condition?)

  (define-condition-type &error &serious 
    make-error error?)

  (define-condition-type &violation &serious
    make-violation violation?)

  (define-condition-type &assertion &violation
    make-assertion-violation assertion-violation?)

  (define-condition-type &irritants &condition
    make-irritants-condition irritants-condition? 
    (irritants condition-irritants))

  (define-condition-type &who &condition 
    make-who-condition who-condition?
    (who condition-who))

  (define-condition-type &non-continuable &violation
    make-non-continuable-violation non-continuable-violation?)

  (define-condition-type &implementation-restriction &violation
    make-implementation-restriction-violation
    implementation-restriction-violation?)

  (define-condition-type &lexical &violation
    make-lexical-violation lexical-violation?)

  (define-condition-type &syntax &violation
    make-syntax-violation syntax-violation?
    (form syntax-violation-form)
    (subform syntax-violation-subform))

  (define-condition-type &undefined &violation
    make-undefined-violation undefined-violation?)

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

  (define-condition-type &i/o-file-protection &i/o
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

  (define-condition-type &no-infinities &implementation-restriction
    make-no-infinities-violation no-infinities-violation?)
  
  (define-condition-type &no-nans &implementation-restriction
    make-no-nans-violation no-nans-violation?) 
)