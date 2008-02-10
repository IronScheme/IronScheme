(import 
  (ironscheme)
  (ironscheme web))

(define (display-html html)
  (define (attribute? x)
    (and (pair? x) (not (or (null? (cdr x)) (pair? (cdr x))))))
  (define (->html html)
    (cond
      [(string? html) (html-encode html)]
      [(null? html) ""]
      [(attribute? html) 
        (if (eq? #t (cdr html))
          (format " ~a" (car html))
          (format " ~a=~s" (car html) (html-encode (cdr html))))]
      [else
        (let-values ([(attrs children) (partition attribute? (cdr html))])
          (if (null? children)
            (format "<~a~a/>\n" 
              (car html)
              (apply string-append (map ->html attrs)))
            (format "<~a~a>\n~a\n</~a>\n" 
              (car html)
              (apply string-append (map ->html attrs)) 
              (apply string-append (map ->html children))
              (car html))))]))
      
  (display (->html html)))
  
(define (method-post?)
  (eqv? (method) "POST"))  
  
(define (method-get?)
  (eqv? (method) "GET"))  
  
(define xhtml "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n")
  

(define title "hello")

(display xhtml)

(display-html 
  `(html (xmlns . "http://www.w3.org/1999/xhtml")
    (head (title ,title))
    (body
      (form (id . "form1") (method . "post")
        (h1 ,title)
        (p "rabble rabble")
				,(if (method-post?)
						'(input (type . "submit") (name . "bar") (value . "Now you can"))
        		'(input (type . "submit") (name . "bar") (value . "Can't click me") (disabled . #t)))
        (br)
        (input (type . "submit") (name . "foo") (value . "Click me!") )
        (br)
        (p "<strong>hello</strong>")
    ))))
