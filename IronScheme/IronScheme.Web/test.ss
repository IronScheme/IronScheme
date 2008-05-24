(import 
  (rnrs)
  (ironscheme format)
  (ironscheme web))

(display "<h1>hello-world</h1>")

(display "<p>")
(display `(id = ,(querystring "id")))
(display "</p>")


(display "<p>")
(display `(verb = ,(method)))
(display "</p>")

(display "<p>")
(display `(form foo = ,(form "foo")))
(display "</p>")


(display "<p>")
(display `(session 1 = ,(session "1")))
(display "</p>")


(display "<p>")
(display `(user-agent = ,(user-agent)))
(display "</p>")

(session-set! "1" 'hello-session)

(display "<form id='form1' method='post'>")
(display "<input type='submit' name='foo' value='Click me!'/>")
(display "</form>")

(define qid (or (querystring "id") 1))

(display (format "<a href='test2.ss?id=~a'>Next test</a>" qid))