(library (controllers web-repl)
  (export
    index)
  (import
    (ironscheme)
    (ironscheme web)
    (ironscheme web controllers)
    (ironscheme web-utils)
    (prefix (views web-repl) view-))
    
  (define (web-env)
    (let ((env (session 'env)))
      (unless env 
        (let ((new-env (new-interaction-environment)))
          (session-set! 'env new-env)
          (set! env new-env)))
      env))
 
  (define-action index
    [(get) 
      (view-index)]
    [(post expr)
      (let ((e (read (open-string-input-port (string-append "(begin " expr ")")))))
        (parameterize [(current-output-port (http-output-port))
                       (current-error-port (http-output-port))]
          (display "<pre>")
          (let ((r (eval e (web-env))))
            (display "</pre>")
            (view-result r))))])  
)    