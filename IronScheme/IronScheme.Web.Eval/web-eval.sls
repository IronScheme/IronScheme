(library (web-eval)
  (export web-eval)
  (import 
    (ironscheme)
    (ironscheme web)
    (ironscheme clr))

  (define (make-stopwatch)
    (clr-static-call System.Diagnostics.Stopwatch StartNew))
    
  (define (elapsed-milliseconds sw)
    (clr-static-call Convert ToInt32 (clr-prop-get System.Diagnostics.Stopwatch ElapsedMilliseconds sw)))
          
  (define (web-eval)
    (let-values (((port extract) (open-string-output-port)))
     (let ((expr (form 'expr)))
       (parameterize [(current-output-port port)
                      (current-error-port port)]
         (guard [e (e (wprintf "{ ~s: ~s, ~s: ~s }" 
                               "error" (format "~a" e) 
                               "output" (extract)))]
            (let ((p (read (open-string-input-port (string-append "(begin " expr "\n)"))))
                  (env (new-interaction-environment))
                  (ms #f))
              (let* ((r (with-timeout 
                         (lambda () 
                           ; parameters are thread static, so bind again... todo: global parameters
                           (parameterize [(current-output-port port)
                                          (current-error-port port)
                                          (interaction-environment env)]
                             (let* ((sw (make-stopwatch))
                                    (r (eval p env)))
                               (set! ms (elapsed-milliseconds sw))
                               r)))
                         5000)))
                (let-values (((p e) (open-string-output-port)))
                  (pretty-print r p)
                  (wprintf "{ ~s: ~s, ~s: ~s, ~s: ~s }" 
                           "output" (extract) 
                           "result" (e)
                           "time" ms)))))))))
    
)