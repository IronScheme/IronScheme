(library (web-eval)
  (export web-eval)
  (import 
    (except (ironscheme) guard)
    (ironscheme syntax)
    (ironscheme web)
    (ironscheme clr))
    
  (clr-using System.Diagnostics)

  (define (make-stopwatch)
    (clr-static-call Stopwatch StartNew))
    
  (define (elapsed-milliseconds sw)
    (clr-static-call Convert ToInt32 (clr-prop-get Stopwatch ElapsedMilliseconds sw)))
    
  (define-syntax-rule (fast-guard [e c] expr)
    (call/cc 
      (lambda (k)
        (with-exception-handler 
          (lambda (e)
            (k ((lambda () c))))
          (lambda ()
            expr)))))
          
  (define (web-eval)
    (response-content-type-set! "application/json")
    (let-values (((port extract) (open-string-output-port)))
     (let ((expr (form 'expr)))
       (parameterize [(current-output-port port)
                      (current-error-port port)]
         (fast-guard [e (wprintf "{ ~s: ~s, ~s: ~s }" 
                               "error" (format "~a" e) 
                               "output" (extract))]
            (let* ((p (read (open-string-input-port (string-append "(begin " expr "\n)"))))
                   (env (new-interaction-environment))
                   (ms (eval '(import (rename (rnrs) (lambda λ))) env)))
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