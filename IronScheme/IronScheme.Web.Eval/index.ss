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
    (display-html
      `(html 
        (head 
          (title "IronScheme")
          (link (rel . "stylesheet") (type . "css/text") (href . "eval.css"))
          (script (type . "text/javascript") (src . "http://code.jquery.com/jquery-1.6.2.min.js") ""))
        (body 
          (form (method . post) (id . eform)
            (textarea (style . "width:500px;height:100px") (id . expr) "")
            (br)
            (input (type . submit)) (a (href . "http://ironscheme.net/doc") (target . _blank) "Help")
            (br)
            (div (id . result) ""))
          (script (type . "text/javascript") (src . "eval.js") ""))))])
      