(import 
  (rnrs)
  (ironscheme web))

(display "<h1>hello-world</h1>")

(display "<p>")
(display `(id = ,(querystring 'id)))
(display "</p>")


(display "<p>")
(display `(verb = ,(http-method)))
(display "</p>")

(display "<p>")
(display `(form foo = ,(form 'foo)))
(display "</p>")


(display "<p>")
(display `(session 1 = ,(session '1)))
(display "</p>")

(session-set! '1 'hello-session)

(display "<form id='form1' method='post'>")
(display "<input type='submit' name='foo' value='Click me!'/>")
(display "</form>")