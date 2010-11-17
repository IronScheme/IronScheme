#| License
Copyright (c) 2007,2008,2009,2010 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme docs topic)
  (export 
    topic?
    topic-id
    topic-description
    topic-name
    topic-form
    topic-params
    topic-return
    topic-exceptions
    topic-remark
    topic-examples
    topic-library
    topic-related
    make-topic)
  (import
    (ironscheme)
    (ironscheme records printer))
  
  (define-record-type topic 
    (fields
      id
      name
      description
      form
      params
      return
      exceptions
      remark
      examples
      library
      related)
    (protocol
      (lambda (p)
        (case-lambda 
          [(n)
					(let ((rest (cdr n))
								(filter1		
									(lambda (s l) 
										(map cdr (filter 
										           (lambda (i) 
											           (and (pair? i) (eq? (car i) s)))
										         l))))
								(assq1			
									(lambda (s l) 
										(let ((x (assq s l))) 
											(if (list? x) 
												(cadr x)
												#f)))))
	          (let
	            ((id      		(make-guid))
	             (name    		(car n))
	             (desc    		(assq1 'description rest))
	             (form    		(assq1 'form rest))
	             (params  		(filter1 'param rest))
	             (return  		(assq1 'return rest))
	             (exceptions  (filter1 'exception rest))
	             (remark  		(assq1 'remark rest))
	             (examples    (filter1 'example rest))
	             (library     (cdr (assq 'library rest)))
	             (related 		(cdr (assq 'related rest))))
	            (p id name desc form params return exceptions remark examples library related)))]
					[(name desc form params return exceptions remark examples library related)
							(p (make-guid) name desc form params return exceptions remark examples library related)]))))		

)
    
