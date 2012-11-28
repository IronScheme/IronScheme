#| 
Nuget test file

Make sure you can call this file from the web browser.

For now IIS (and IIS Express) works in integrated mode (only tested on .NET 4)

This file aims to do absolutely nothing but test if the IronScheme web handler works. It can be deleted if not needed.
|#

(import 
  (ironscheme)
  (ironscheme web))
  
(define (method-post?)
  (eq? (http-method) 'post))  
  
(define (method-get?)
  (eq? (http-method) 'get))  
  
(define counter (string->number (or (querystring "id") "1")))
  
(define xhtml "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n")

(define title "hello world")

(display xhtml)

(display-html 
  `(html (xmlns . "http://www.w3.org/1999/xhtml")
    (head (title ,title))
    (body
      (form (id . "form1") (method . "post")
        (h1 ,title)
        (p "rabble rabbles")
				,(if (and (method-post?) (form "foo"))
						'(input (type . "submit") (name . "bar") (value . "Now you can"))
        		'(input (type . "submit") (name . "bar") (value . "Can't click me") (disabled . #t)))
        (br)
        (input (type . "submit") (name . "foo") (value . "Click me!") )
        (br)
        (p "bar = " ,(form "bar"))
        (p "<strong>hello</strong>")
        (p "baz = " ,(form "baz"))
        (select (name . "baz") 
          ,@(map (lambda (x) 
                   `(option ,x (selected . ,(eqv? (form "baz") x))))
              '("good" "bad" "ugly") ) 
          (onchange . "submit()"))
        (p (a (href . ,(string-format "test.ss?id={0}" (+ 1 counter))) "Go back"))
        (p (a (href . ,(string-format "test.ss?id={0}" (+ 100 counter))) "Go forward"))))))