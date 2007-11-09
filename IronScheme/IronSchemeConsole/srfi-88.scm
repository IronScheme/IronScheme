 
(define real-symbol? symbol?)
(define real-symbol->string symbol->string)
(define real-string->symbol string->symbol)

(define looks-like-an-unquoted-keyword?
  (lambda (s)
    (let ((n (string-length s)))
      (and (> n 1)
           (char=? (string-ref s (- n 1)) #\:)))))

(set! symbol?
  (lambda (obj)
    (and (real-symbol? obj)
         (not (looks-like-an-unquoted-keyword?
               (real-symbol->string obj))))))

(define keyword?
  (lambda (obj)
    (and (real-symbol? obj)
         (looks-like-an-unquoted-keyword?
          (real-symbol->string obj)))))

(set! symbol->string real-symbol->string)

(define keyword->string
  (lambda (k)
    (let* ((s (real-symbol->string k))
           (n (string-length s)))
      (substring s 0 (- n 1))))) ; remove the colon

(set! string->symbol
  (lambda (s)
    (if (looks-like-an-unquoted-keyword? s)
        (error "sorry... the symbol would look like a keyword!")
        (real-string->symbol s))))

(define string->keyword
  (lambda (s)
    (let ((s-colon (string-append s ":")))
      (if (looks-like-an-unquoted-keyword? s-colon)
          (real-string->symbol s-colon)
          (error "sorry... the keyword would look like a symbol!")))))
