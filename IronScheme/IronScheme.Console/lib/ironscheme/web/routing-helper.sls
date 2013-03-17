#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme web routing-helper)
  (export
    match)
  (import 
    (ironscheme)
    (ironscheme regex))
    
  (define-syntax match
    (lambda (x)
      (define (get-ids tmp) 
        (map 
          (lambda (e)
            (let ((e* (syntax->datum e)))
              (cond
                [(and (symbol? e*) (not (memq e* '(_ ...)))) e]
                [(string? e*)
                  (car (generate-temporaries #'(e)))]
                [else
                  (syntax-case e ()
                    [(n v) (identifier? #'n) #'n]
                    [_ (syntax-violation 'match 
                        "not a valid id, string or grouping" e)])])))
          tmp))
      (define (get-match tmp)
        (map
          (lambda (e)
            (syntax-case e ()
              [(n v) #'n]
              [n #'n]))
          tmp))
      (define (get-fender f tmp)
        #`(and 
          #,@(remq #f 
              (map
                (lambda (e)
                  (syntax-case e ()
                    [(n v) 
                        (let ((v* (syntax->datum #'v)))
                          (if (string? v*) 
                              #`(regex-match? n #,(string-append "^" v* "$"))
                              (syntax-violation 'match "not a string" #'v e)))]
                    [n #f]))
                tmp)) #,f ))
      (define (parse-clause e)
        (lambda (c)
          (with-syntax ((e e))
            (syntax-case c ()
              [((m ...) c)
                ((parse-clause #'e) #'((m ...) #t c))]
              [((m ...) f c)
                (with-syntax (((i ...) (get-ids #'(m ...)))
                              ((m ...) (get-match #'(m ...)))
                              (f (get-fender #'f #'(m ...))))
                  #'((m ...) 
                      (apply (lambda (i ...) f) e)
                      (apply (lambda (i ...) c) e)))]))))
      (syntax-case x ()
        [(_ e c ...)
          (with-syntax ((e* (car (generate-temporaries #'(e)))))
            (with-syntax (((c ...) (map (parse-clause #'e*) #'(c ...))))
              #'(let ((e* e))
                  (syntax-case e* ()
                    c ...
                    [_ #f]))))]))))