(import 
  (rnrs)
  (ironscheme format)
  (ironscheme web))
  
(define (render obj)
  (display obj (http-output-port)))  

(render "<h1>hello-world</h1>")

(render "<p>")
(render `(id = ,(querystring "id")))
(render "</p>")


(render "<p>")
(render `(verb = ,(method)))
(render "</p>")

(render "<p>")
(render `(form foo = ,(form "foo")))
(render "</p>")


(render "<p>")
(render `(session 1 = ,(session "1")))
(render "</p>")


(render "<p>")
(render `(user-agent = ,(user-agent)))
(render "</p>")

(session-set! "1" 'hello-session)

(render "<form id='form1' method='post'>")
(render "<input type='submit' name='foo' value='Click me!'/>")
(render "</form>")

(define qid (or (querystring "id") 1))

(render (format "<a href='test2.ss?id=~a'>Next test</a>" qid))