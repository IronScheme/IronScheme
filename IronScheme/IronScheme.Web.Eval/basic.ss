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
     (let ((expr (form 'expr))
           (port (http-output-port)))
       (parameterize [(current-output-port port)
                      (current-error-port port)]
         (display "<pre class='output'>")
         (guard [e (e (display (html-encode (format "~a" e)))
                      (display "</pre>"))]
            (let ((p (read (open-string-input-port (string-append "(begin " expr ")"))))
                  (env (web-env)))
              (let ((r (with-timeout 
                         (lambda () 
                           ; parameters are thread static, so bind again... todo: global parameters
                           (parameterize [(current-output-port port)
                                          (current-error-port port)]
                             (eval p env))) 2000)))
                (display "</pre>")
                (display "<pre class='result'>")
                (let-values (((p e) (open-string-output-port)))
                  (pretty-print r p)
                  (display (html-encode (e)))
                  (display "</pre>")))))))]
  [(get)
    (display "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
             (http-output-port))  
    (display-html
      `(html  (xmlns . "http://www.w3.org/1999/xhtml")
        (head 
          (title "IronScheme")
          (link (rel . "stylesheet") (type . "text/css") (href . "eval.css"))
          (script (type . "text/javascript") (src . "http://code.jquery.com/jquery-1.6.2.min.js") ""))
        (body 
          (form (method . post) (id . eform)
            (textarea (style . "width:500px;height:100px") (id . expr) "")
            (br)
            (input (type . submit)) (a (href . "http://ironscheme.net/doc") (target . _blank) "Help")
            (br)
            (div (id . result) ""))
          (script (type . "text/javascript") (src . "eval-basic.js") ""))))])