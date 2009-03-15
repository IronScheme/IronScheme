(library (clr)
  (export import-clr-method)
  (import 
    (ironscheme)
    (ironscheme linq)
    (ironscheme strings)
    (ironscheme clr)
    (ironscheme clr reflection))


  (define-syntax import-clr
    (lambda (x)
      (syntax-case x ()
        [_ #f])))
        
  (define (enum? obj)
    (or
      (symbol? obj)
      (and (list? obj) (for-all symbol? obj))))
        
  (define-syntax import-clr-method
    (lambda (x)
      (define (get-params meth)
        (let ((p (method-params meth)))
          (let ((r  (map
                      (lambda (p)
                        (let ((name (param-name p))
                              (type (param-type p)))
                          (cons name type)))
                       p)))
              (if (method-static? meth)
               r
               (cons (cons "this" (member-declaring-type meth)) r))))) 
      (define (get-methods type name)
        (type-member type (symbol->string name) 'method '(instance public static)))
      (define (get-methdef type meth params)
        (let ((static? (method-static? meth))
              (p       (datum->syntax type (map string->symbol (map car params)))))
          (with-syntax ((p    p)
                        (type type)
                        (call (if static? #'clr-static-call #'clr-call))
                        (meth (string-append 
                                (member-name meth)
                                "("
                                (string-join "," (map type-fullname (map cdr (if static? params (cdr params)))))
                                ")")))
            #'(call type meth . p))))
      (define (get-choices type meths)
        (with-syntax (((p ...) (datum->syntax type (map string->symbol (map car (cdar meths)))))
                      (name    (datum->syntax type (string-append 
                                                      (symbol->string (syntax->datum type))
                                                      "::"
                                                      (member-name (caar meths)))))
                      (((methdef check ...) ...)
          (map (lambda (meth)
                 (let ((fp (map (lambda (o n) (cons (car n) (cdr o))) (cdr meth) (cdar meths))))
                   (cons (get-methdef type (car meth) fp)
                     (datum->syntax type 
                      (map 
                        (lambda (e) 
                          (let ((n (string->symbol (car e)))
                                (t (cdr e)))
                            (if (type-enum? t)
                              `(enum? ,n)
                              (let ((fn (string->symbol (type-fullname t)))
                                    (vt? (type-valuetype? t)))
                                (cond
                                  [(eq? n 'this)           #t]
                                  [(eq? fn 'System.Object) #t]
                                  [vt?                     `(clr-is ,fn ,n)]
                                  [else                    `(or (null? ,n) (clr-is ,fn ,n))])))))
                          fp)))))
            meths)))
          #'((p ...)
              (cond 
                [(and check ...) methdef] ...
                [else
                  (assertion-violation name "invalid arguments" p ...)]))))
      (syntax-case x ()
        [(_ type method)
         (let* ((t (get-clr-type (syntax->datum #'type)))
                (m (get-methods t (syntax->datum #'method)))
                (p (map get-params m))
                (a (map cons m p)))
           (with-syntax (((meth ...) (map   
                                      (lambda (m) 
                                        (get-choices #'type (get-list m)))
                                      (from md in a group md by (length md)))))              
             #'(case-lambda
                 meth ...)))]
        ))) 

)

;;tests
  (define dt-parse (import-clr-method System.DateTime Parse))     
  (define str-cmp  (import-clr-method System.String Compare))
  ;(define ts (import-clr-method System.String TrimStart))
        
  ;(import-clr-method System.Math Sqrt)           
  ;(import-clr-method System.Math Abs)
  ;(import-clr-method System.Math Log)

  ;(import-clr-method System.Object ToString)
  ;(import-clr-method System.Object Equals)

  (import-clr-method System.Reflection.Emit.ILGenerator Emit)


  ;(display (ts "(hello" '#(#\())) ;not char[], how to do?
  ;(newline)


  (time-it "calls" 

  (lambda()

  (display (dt-parse "12:00:00" '() 'assumelocal)) ; null is '()
  (newline)




  (display (str-cmp "hello" 1 "worEL" 3 2 #t))
  (newline)

  (display (str-cmp "hello" 1 "worEL" 3 2 'ordinalignorecase))
  (newline)))


