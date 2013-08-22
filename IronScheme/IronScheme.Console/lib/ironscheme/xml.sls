#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme xml)
  (export
    ->xml
    string->xml)
  (import 
    (ironscheme)
    (ironscheme clr)
    (ironscheme web-utils))
    
  (define string->xml (symbol-value 'string->xml))    
  
  (define (string-concat . args)
    (clr-static-call String (Concat Object[]) (list->vector args)))

  ; TODO: attempt to make this a macro
	(define ->xml
		(letrec 
			((complex? 				
			  (lambda (x)     
			    (and (find list? x))))
			 (attribute?			
			  (lambda (x)     
			    (and (pair? x) (not (or (null? (cdr x)) (pair? (cdr x)))))))
			 (map1					  
			  (lambda (i l)   
			    (map (lambda (x) i) l)))
			 (string-map			
			  (lambda (f l i) 
			    (apply string-concat (map f l (map1 (fx+ 1 i) l)))))
			 (get-indent			
			  (lambda (i)     
			    (make-string (fx* 2 i) #\space)))
			 (->xml           
				(lambda (x i)
		      (cond
		        [(string? x) (html-encode x)]
		        [(null? x) ""]
		        [(unspecified? x) ""]
		        [(not (pair? x)) (html-encode (format "~a" x))]
		        [(attribute? x)
		          (let ((name (car x))
		                (value (cdr x))) 
		            (if (boolean? value)
		                (if (eq? #t value)
		                    (format " ~a" name)
		                    "")
		                (format " ~a=~s" name (html-encode (format "~a" value)))))]
		        [else
		          (let ((tag (car x))
		                (body (cdr x))
		                (indent-str (get-indent i)))
		            (if (eq? tag 'no-escape)
		                (car body)
		                (let-values ([(attrs children) (partition attribute? body)])
		                  (if (null? children)
		                      (format "~a<~a~a />\n"
		                        indent-str 
		                        tag
		                        (string-map ->xml attrs i))
		                      (if (complex? x) 
		                          (format
		                            "~a<~a~a>\n~a~a</~a>\n"
		                            indent-str
		                            tag
		                            (string-map ->xml attrs i) 
		                            (string-map ->xml children i)
		                            indent-str
		                            tag)                    
		                          (format
		                            "~a<~a~a>~a</~a>\n"
		                            indent-str
		                            tag
		                            (string-map ->xml attrs i) 
		                            (string-map ->xml children i)
		                            tag))))))]))))
				(lambda (x)
					(->xml x 0)))))
