(library (ironscheme constant-fold)
  (export 
    enable-constant-fold
    enable-constant-fold/env)
  (import (ironscheme)
          (ironscheme clr))
  
  (define (allow-constant-fold? proc)
    (clr-prop-get IronScheme.Runtime.ICallable AllowConstantFold proc))

  (define (set-allow-constant-fold! proc bool)
    (clr-prop-set! IronScheme.Runtime.ICallable AllowConstantFold proc bool))
    
  (define (enable-constant-fold . procs)
    (for-each (lambda (proc)
                (set-allow-constant-fold! proc #t))
              procs))
              
  (define (proc-filter b)
    (eq? 'procedure (cdr b)))
              
  (define (enable-constant-fold/env . import-spec)
    (let ((env (apply environment import-spec))
          (env* (apply environment '(only (rnrs) list) import-spec)))
      (let ((b (map car (filter proc-filter (environment-bindings env)))))
        (apply enable-constant-fold (eval `(list ,@b) env*)))))
        
)          
  