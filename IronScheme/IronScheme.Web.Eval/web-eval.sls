(library (web-eval)
  (export web-eval)
  (import 
    (ironscheme)
    (ironscheme web))

  (define (web-env)
    (new-interaction-environment))
      
  (define (render s)
    (display s (http-output-port)))
    
  (define (web-eval)
    (let-values (((port extract) (open-string-output-port)))
     (let ((expr (form 'expr)))
       (parameterize [(current-output-port port)
                      (current-error-port port)]
         (guard [e (e (render (format "{ ~s: ~s, ~s: ~s }" 
                                      "error" (format "~a" e) 
                                      "output" (extract))))]
            (let ((p (read (open-string-input-port (string-append "(begin " expr ")"))))
                  (env (web-env)))
              (let ((r (with-timeout 
                         (lambda () 
                           ; parameters are thread static, so bind again... todo: global parameters
                           (parameterize [(current-output-port port)
                                          (current-error-port port)
                                          (interaction-environment env)]
                             (eval p env))) 2000)))
                (let-values (((p e) (open-string-output-port)))
                  (pretty-print r p)
                  (render (format "{ ~s: ~s, ~s: ~s }" 
                                  "output" (extract) 
                                  "result" (e)))))))))))
    
)