(library (ironscheme conditions)
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

)