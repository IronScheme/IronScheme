#| License
Copyright (c) 2007-2016 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme conditions)
  (export
    &condition
    condition
    simple-conditions
    condition?
    condition-predicate
    condition-accessor
  
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
    
    &no-infinities
    make-no-infinities-violation
    no-infinities-violation?
    
    &no-nans
    make-no-nans-violation
    no-nans-violation?    
    
    &where
    make-where-condition
    where-condition?
    condition-where
    
    &stacktrace
    make-stacktrace-condition
    stacktrace-condition?
    condition-stacktrace
    
    &condition-rtd &condition-rcd &message-rtd &message-rcd
    &warning-rtd &warning-rcd &serious-rtd &serious-rcd
    &error-rtd &error-rcd &violation-rtd &violation-rcd
    &assertion-rtd &assertion-rcd &irritants-rtd
    &irritants-rcd &who-rtd &who-rcd &non-continuable-rtd
    &non-continuable-rcd &implementation-restriction-rtd
    &implementation-restriction-rcd &lexical-rtd &lexical-rcd
    &syntax-rtd &syntax-rcd &undefined-rtd &undefined-rcd   
    
    &i/o-rtd &i/o-rcd &i/o-read-rtd &i/o-read-rcd
    &i/o-write-rtd &i/o-write-rcd &i/o-invalid-position-rtd
    &i/o-invalid-position-rcd &i/o-filename-rtd
    &i/o-filename-rcd &i/o-file-protection-rtd
    &i/o-file-protection-rcd &i/o-file-is-read-only-rtd
    &i/o-file-is-read-only-rcd &i/o-file-already-exists-rtd
    &i/o-file-already-exists-rcd &i/o-file-does-not-exist-rtd
    &i/o-file-does-not-exist-rcd &i/o-port-rtd &i/o-port-rcd
    &i/o-decoding-rtd &i/o-decoding-rcd &i/o-encoding-rtd
    &i/o-encoding-rcd &no-infinities-rtd &no-infinities-rcd
    &no-nans-rtd &no-nans-rcd 
    
    &where-rtd &where-rcd
    &stacktrace-rtd &stacktrace-rcd)
  (import 
    (except (rnrs)
      &condition
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
      
      &no-infinities
      make-no-infinities-violation
      no-infinities-violation?
      
      &no-nans
      make-no-nans-violation
      no-nans-violation?))
  
  (define-record-type &condition (nongenerative))
  
  (define &condition-rtd (record-type-descriptor &condition))
  (define &condition-rcd (record-constructor-descriptor &condition)) 
  
  (define-syntax define-condition-type
    (lambda (x)
      (define (mkname name suffix)
        (datum->syntax name 
           (string->symbol 
             (string-append 
               (symbol->string (syntax->datum name))
               suffix))))
      (syntax-case x ()
        [(ctxt name super constructor predicate (field* accessor*) ...)
         (and (identifier? #'name) 
              (identifier? #'super)
              (identifier? #'constructor)
              (identifier? #'predicate)
              (for-all identifier? #'(field* ...))
              (for-all identifier? #'(accessor* ...)))
         (with-syntax ([(aux-accessor* ...) (generate-temporaries #'(accessor* ...))]
                       [rtd (mkname #'name "-rtd")]
                       [rcd (mkname #'name "-rcd")])
            #'(begin
               (define-record-type (name constructor p?)
                  (parent super)
                  (fields (immutable field* aux-accessor*) ...)
                  (nongenerative)
                  (sealed #f) 
                  (opaque #f))
               (define predicate (condition-predicate (record-type-descriptor name)))
               (define accessor* (condition-accessor (record-type-descriptor name) aux-accessor*)) 
               ...
               (define rtd (record-type-descriptor name))
               (define rcd (record-constructor-descriptor name))))])))  
  
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
    
  (define-condition-type &where &condition 
    make-where-condition where-condition?
    (where condition-where))   
    
  (define-condition-type &stacktrace &condition 
    make-stacktrace-condition stacktrace-condition?
    (stacktrace condition-stacktrace))  

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
    
  (define-condition-type &no-infinities &implementation-restriction
    make-no-infinities-violation no-infinities-violation?)
  
  (define-condition-type &no-nans &implementation-restriction
    make-no-nans-violation no-nans-violation?))
