(library (docs templates)
  (export topic->maml)
  (import
    (ironscheme)
    (ironscheme format)
    (docs topic)
    (docs topic-manager)
    (ironscheme xml))

  (define (flatten list)
    (apply append list))

  (define (desc t)
    (let ((d (topic-description t)))
      (if d d "No information provided")))

  (define (return t)
    (let ((d (topic-return t)))
      (if d `(returnValue (content (para ,d))))))

  (define (param p)
    `((definedTerm (legacyItalic ,(symbol->string (car p))))
      (definition (para ,(cadr p)))))

  (define (params t)
    (let ((d (topic-params t)))
      (unless (null? d)
        `(parameters
          (content
            (definitionTable ,@(flatten (map param d))))))))

  (define (exception e)
    `((entry (para (languageKeyword ,(symbol->string (car e)))))
      (entry (para ,(cadr e)))))
            
  (define (exceptions t)
    (let ((d (topic-exceptions t)))
      (unless (null? d)
        `(exceptions
          (content
            (table
              (row ,@(flatten (map exception d)))))))))
              
  (define (remark t)
    (let ((d (topic-remark t)))
      (if d `(languageReferenceRemarks (content (para ,d))))))

	(define (make-link d)
		`(link (xlink:href . ,(format "~a" (get-topic-id d))) "."))
		
  (define (lib l)
    `(para ,(make-link l)))
	      
  (define (libraries t)
    (let ((d (topic-library t)))
      (if d `(requirements (content ,@(map lib d))))))
      
  (define (get-code c)
    (let ((cs (format "~a" c)))
      (substring cs 1 (fx- (string-length cs) 1))))
      
  (define (example e)
    (if (string? (car e))
      `(codeExample
        (description (content (para ,(car e))))
        (code ,(get-code (cdr e))))
      `(codeExample
        (code ,(get-code e)))))
      
  (define (examples t)
    (let ((d (topic-examples t)))
      (unless (null? d)
        (map example d))))
      
  (define (topic->maml t)
    (assert (topic? t))
    (->xml
      `(topic (id . ,(topic-id t))
        (developerReferenceWithSyntaxDocument
          (xmlns . "http://ddue.schemas.microsoft.com/authoring/2003/5")
          (xmlns:xlink . "http://www.w3.org/1999/xlink")
          (summary (para (languageKeyword ,(symbol->string (topic-name t)))))
          (introduction (para ,(desc t)))
          (syntaxSection (legacySyntax ,(format "~a" (topic-form t))))
          ,(params t)
          ,(return t)
          ,(exceptions t)
          ,(remark t)
          ,@(examples t)
					,(libraries t)
          (relatedTopics ,@(map make-link (topic-related t)))))))

)

#|
What we need for a topic

* name
- generated guid
* description?
* syntax/form
- parameters - name
             - description?
- return
- exceptions? - type
              - description
- remarks?
- example? - description?
           - code
* related - guids?

|#
