(import 
  (ironscheme)
  (ironscheme web))

(define (web-env)
  (let ((env (session 'env)))
    (unless env 
      (let ((new-env (new-interaction-environment)))
        (session-set! 'env new-env)
        (set! env new-env)))
    env))
  
(case (http-method)
  [(post)
     (let ((expr (form 'expr)))
       (parameterize [(current-output-port (http-output-port))
                      (current-error-port (http-output-port))]
         (display "<pre class='output'>")
         (guard [e (e (display (html-encode (format "~a" e)))
                      (display "</pre>"))]
            (let ((p (read (open-string-input-port (string-append "(begin " expr ")"))))
                  (env (web-env)))
              (let ((r (with-timeout (lambda () (eval p env)) 5000)))
                (display "</pre>")
                (display "<pre class='result'>")
                (let-values (((p e) (open-string-output-port)))
                  (pretty-print r p)
                  (display (html-encode (e)))
                  (display "</pre>")))))))]
  [(get)
    (display "<!DOCTYPE html>\n" (http-output-port))
    (display-html
      `(html 
        (head 
          (title "IronScheme")
          (meta (name .  viewport) (content . "width=device-width, initial-scale=1"))
          (link (rel . "stylesheet") (type . "css/text") (href . "http://code.jquery.com/mobile/1.0b2/jquery.mobile-1.0b2.min.css"))
          (script (type . "text/javascript") (src . "http://code.jquery.com/jquery-1.6.2.min.js") "")
          (script (type . "text/javascript") (src . "http://code.jquery.com/mobile/1.0b2/jquery.mobile-1.0b2.min.js") "")
          (link (rel . "stylesheet") (type . "css/text") (href . "eval.css"))
          (script (type . "text/javascript") (src . "ga.js") ""))
        (body 
          (form (method . post) (id . eform)
            (div (data-role . page) 
              (div (data-role . header) (h1 "IronScheme"))
              (div (data-role . content) 
                (div (id . result) "")
                (textarea (id . expr) ""))
              (div (data-role . footer) (data-position . fixed)
                (h4 (input (type . submit) (value . "Submit") (click . "return docall();" ))))
              (script (type . "text/javascript") (src . "eval.js") ""))))))])
      