(library (ironscheme generic-writer)
  (export 
    write 
    display
    initialize-default-printers
    generic-write
    add-custom-printer!
    add-record-printer!)
  (import 
    (except (ironscheme) write-char write display generic-write initialize-default-printers)
    (ironscheme clr))
    
  (define write
    (case-lambda 
      [(obj)    
        (generic-write obj #t)]
      [(obj port)    
        (generic-write obj port #t)]))

  (define display
    (case-lambda 
      [(obj)    
        (generic-write obj #f)]
      [(obj port)    
        (generic-write obj port #f)]))
  
  (define generic-write 
    (case-lambda
      [(obj)
        (generic-write obj #t)]
      [(obj readable?)
        (generic-write obj (current-output-port) readable?)]
      [(obj port readable?)
        (cond
          [(null? obj)        (put-string port "()")]
          [(boolean? obj)     (put-string port (if obj "#t" "#f"))]
          [(number? obj)      (put-string port (number->string obj))]
          [(eof-object? obj)  (put-string port "#<eof>")]
          [(unspecified? obj) (put-string port "#<unspecified>")]
          [else (or (write-custom obj port readable?)
                    (write-custom-printers record-printers obj port readable?)
                    (write/type obj port))])
        (void)]))
          
  (define custom-printers '())
  (define record-printers '())
    
  (define (add-custom-printer! pred printer)
    (let ((existing (assq pred custom-printers)))
      (unless existing
        (set! custom-printers (append custom-printers (list (cons pred printer)))))))
    
  (define (add-record-printer! pred printer)
    (set! record-printers (append record-printers (list (cons pred printer)))))
    
  (define (write-custom-printers printers obj port readable?)
    (let f ((p printers))
      (if (null? p)
          #f
          (let ((pred (caar p)) (printer (cdar p)))
            (if (pred obj)
                (begin (printer obj port readable?) #t)
                (f (cdr p)))))))    
    
  (define (write-custom obj port readable?)
    (write-custom-printers custom-printers obj port readable?))
    
  (define (initial-printers)
    (add-custom-printer! symbol? write-symbol)
    (add-custom-printer! char? write-char)
    (add-custom-printer! string? write-string)
    (add-custom-printer! pair? write-pair)
    (add-custom-printer! procedure? write-procedure)
    (add-custom-printer! bytevector? write-bytevector)
    (add-custom-printer! vector? write-vector))
                
  (define (initialize-default-printers)
    (let ((p custom-printers))
      (set! custom-printers '())
      (initial-printers)
      (add-custom-printer! port? write-port)
      (add-custom-printer! condition? write-condition)
      (add-custom-printer! record? write-record)
      (add-custom-printer! multiple-values? write-mv)
      (for-each 
        (lambda (p)
          (add-custom-printer! (car p) (cdr p)))
        p)))
        
  (define (get-clr-type-name obj)
    (clr-prop-get System.Type Name (clr-call System.Object GetType obj)))
        
  (define (write-procedure proc port readable?)
    (let ((pn (procedure-name proc)))
      (if pn
          (begin
            (put-string port "#<procedure ")
            (write-symbol pn port readable?)
            (put-string port ">"))
          (put-string port "#<procedure>"))))
        
        
  (define (multiple-values? obj)
    (clr-is IronScheme.Runtime.MultipleValues obj))
    
  (define (write-mv obj port readable?)
    (let f ((vals (call-with-values (lambda () obj) list)))
      (cond
        [(null? vals)]
        [(null? (cdr vals))
          (generic-write (car vals) port readable?)]
        [else
          (generic-write (car vals) port readable?)
          (newline port)
          (f (cdr vals))])))
        
  (define (write-symbol sym port readable?)
    (if readable?
        (let* ((str (symbol->string sym))
               (len (string-length str)))
          (let f ((i 0))
            (cond
              [(= i len)]
              [else
                (let* ((chr (string-ref str i))
                       (cat (char-general-category chr)))
                  (cond 
                    [(or (and (zero? i) 
                         (or (eq? cat 'Nd)
                             (char=? chr #\@)
                             (and (char=? chr #\.) (not (string=? str "...")))))
                         (memq cat '(Cn Zs)))
                      (put-string port "\\x")
                      (put-string port (number->string (char->integer chr) 16))
                      (put-string port ";")]
                    [else
                      (put-char port chr)]))
                (f (+ i 1))])))
        (put-string port (symbol->string sym))))
          
  (define (write-char chr port readable?)
    (if readable?
        (case (char->integer chr)
          [(0) (put-string port "#\\nul")]
          [(7) (put-string port "#\\alarm")]
          [(8) (put-string port "#\\backspace")]
          [(9) (put-string port "#\\tab")]
          [(10) (put-string port "#\\newline")]
          [(11) (put-string port "#\\vtab")]
          [(12) (put-string port "#\\page")]
          [(13) (put-string port "#\\return")]
          [(27) (put-string port "#\\esc")]
          [(32) (put-string port "#\\space")]
          [(127) (put-string port "#\\delete")]
          [else (put-string port "#\\")
                (put-char port chr)])
        (put-char port chr)))
      
  (define (write-string str port readable?)
    (if readable?
        (let ((len (string-length str)))
          (put-char port #\")
          (let f ((i 0))
            (unless (= i len)
              (let ((chr (string-ref str i)))
                (case chr
                  [(#\\) (put-string port "\\\\")]
                  [(#\") (put-string port "\\\"")]
                  [(#\newline) (put-string port "\\n")]
                  [(#\return) (put-string port "\\r")]
                  [(#\tab) (put-string port "\\v")]
                  [else (put-char port chr)])
                (f (+ i 1)))))
          (put-char port #\"))
        (put-string port str)))

  (define (write-pair p port readable?)
    (define (write-normal)
      (put-string port "(")
      (let f ((ar (car p))(dr (cdr p)))
        (cond 
          [(null? dr) (generic-write ar port readable?)]
          [(pair? dr)
            (generic-write ar port readable?)
            (put-string port " ")
            (f (car dr) (cdr dr))]
          [else
            (generic-write ar port readable?)
            (put-string port " . ")
            (generic-write dr port readable?)]))
      (put-string port ")"))
    (let ((ar (car p)) (dr (cdr p)))
      (define (write-short str)
        (put-string port str) 
        (generic-write (car dr) port readable?))
      (if (and (symbol? ar) (pair? dr) (null? (cdr dr)))
          (case ar
            [(quote)              (write-short "'")]
            [(quasiquote)         (write-short "`")]
            [(unquote)            (write-short ",")]
            [(unquote-splicing)   (write-short ",@")]
            [(syntax)             (write-short "#'")]
            [(quasisyntax)        (write-short "#`")]
            [(unsyntax)           (write-short "#,")]
            [(unsyntax-splicing)  (write-short "#,@")]
            [else (write-normal)])
          (write-normal))))
      
  (define (write-vector vec port readable?)
    (put-string port "#(")
    (let* ((len (vector-length vec))
           (len-1 (- len 1)))       
      (let f ((i 0))
        (cond
          [(= i len)]
          [(= i len-1)
            (generic-write (vector-ref vec i) port readable?)]
          [else
            (generic-write (vector-ref vec i) port readable?)
            (put-string port " ")
            (f (+ i 1))])))
    (put-string port ")"))
      
  (define (write-bytevector bv port readable?)
    (put-string port "#vu8(")
    (let* ((len (bytevector-length bv))
           (len-1 (- len 1)))       
      (let f ((i 0))
        (cond
          [(= i len)]
          [(= i len-1)
            (generic-write (bytevector-u8-ref bv i) port readable?)]
          [else
            (generic-write (bytevector-u8-ref bv i) port readable?)
            (put-string port " ")
            (f (+ i 1))])))
    (put-string port ")"))
      
  (define (write-port p port readable?)
    (put-string port "#<")
    (if (binary-port? p)
        (put-string port "binary-")
        (put-string port "textual-"))
    (if (input-port? p)
        (if (output-port? p)
            (put-string port "input/output-")
            (put-string port "input-"))
        (put-string port "output-"))
    (put-string port "port>"))
    
  (define (get-field-pairs rtd rec)
    (let* ((flds (record-type-field-names rtd))
           (len  (vector-length flds)))
      (let f ((i 0)(a '()))
        (if (= i len)
            (reverse a)
            (f (+ i 1) 
               (cons (cons (vector-ref flds i) 
                           ((record-accessor rtd i) rec))
                     a))))))
    
  (define (get-fields rtd rec)
    (let ((par (record-type-parent rtd)))
      (if par
          (append (get-fields par rec) (get-field-pairs rtd rec))
          (get-field-pairs rtd rec))))
          
  (define (write-record rec port readable?)
    (define (write-record-generic)
      (let ((rtd (record-rtd rec)))
        (if rtd
            (let ((name (record-type-name rtd)))
              (put-string port "#<")
              (generic-write name port readable?)
              (for-each 
                (lambda (nv)
                  (put-string port " ")
                  (generic-write (car nv) port readable?)
                  (put-string port ":")
                  (generic-write (cdr nv) port readable?))
                (get-fields rtd rec))
              (put-string port ">"))
            (put-string port "#<unknown-record>")))) ; will the last expr ever be hit?
    (or (write-custom-printers record-printers rec port readable?)
        (write-record-generic)))
        
  (define (clr-exception-message ex)
    (clr-prop-get System.Exception Message ex))
        
  (define (display-condition e port)
    (for-each 
      (lambda (c)
        (let ((rtd (record-rtd c)))
          (if rtd
              (begin
                (put-string port (symbol->string (record-type-name rtd)))
                (let* ((flds (get-fields rtd c))
                       (len  (length flds)))
                  (cond 
                    [(zero? len)
                      (put-string port "\n")]
                    [(= 1 len)
                      (let ((fld (cdr (car flds))))
                        (if (vector? fld)
                            (let ((len (vector-length fld)))
                              (put-string port "\n")
                              (let f ((i 0))
                                (cond
                                  [(= i len)]
                                  [else
                                    (put-string port "  [")
                                    (put-string port (number->string (+ i 1)))
                                    (put-string port "] ")
                                    (generic-write (vector-ref fld i) port #t)
                                    (put-string port "\n")
                                    (f (+ i 1))]))) 
                            (begin
                              (put-string port ": ")
                              (generic-write fld port #t)
                              (put-string port "\n"))))]
                    [else
                      (put-string port ":\n")
                      (for-each
                        (lambda (nv)
                          (put-string port "  ")
                          (generic-write (car nv) port #f)
                          (put-string port ": ")
                          (generic-write (cdr nv) port #t)
                          (put-string port "\n"))
                        flds)])))
              (let ((name (get-clr-type-name c))
                    (msg  (clr-exception-message c)))
                (put-string port "#<clr-exception ")
                (put-string port name)
                (put-string port " ")
                (put-string port msg)
                (put-string port ">")))))
      (simple-conditions e)))

  (define (write-condition cnd port readable?)
    (if readable?
        (let ((cnds (simple-conditions cnd)))
          (if (= 1 (length cnds))
              (write-record (car cnds) port readable?)
              (put-string port "#<compound-condition>")))
        (display-condition cnd port)))
      
  (define (write/type obj port)
    (let ((name (get-clr-type-name obj)))
      (put-string port "#<clr-type ")
      (put-string port name)
      (let ((s (clr-call Object ToString obj)))
        (unless (string=? s name)
          (put-string port " ")
          (put-string port s)))
      (put-string port ">")))
      
  (initial-printers))
  
