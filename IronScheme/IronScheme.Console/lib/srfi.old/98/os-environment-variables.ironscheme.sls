(library (srfi :98 os-environment-variables)
  (export
    get-environment-variables
    get-environment-variable)
  (import
    (ironscheme)
    (except (ironscheme environment) get-environment-variables)
    (rename (ironscheme environment) (get-environment-variables get-env-vars)))
    
  (define (get-environment-variables)    
    (let-values (((k v) (hashtable-entries (get-env-vars))))
      (map cons (vector->list k) (vector->list v))))
)
