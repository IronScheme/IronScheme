#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme contracts-helper)
  (export
    parse-body)
  (import 
    (ironscheme) 
    (only (ironscheme unsafe) $car $cdr)
    (ironscheme clr))
    
  (define (string-split str . del)
    (clr-call String (Split String[] StringSplitOptions) str (list->vector del) 'None))
    
  (define (parse-body loc x)
    (define (get-name/type name)
      (let ((tokens (string-split 
                      (symbol->string (syntax->datum name)) 
                      ":")))
        (when (zero? (string-length (vector-ref tokens 0)))
          (syntax-violation 'get-name/type "length of argument > 0" name))
        (if (= 1 (vector-length tokens))
            (cons 
              (datum->syntax name
                (string->symbol (vector-ref tokens 0))) #f)
            (cons* 
              (datum->syntax name
                (string->symbol (vector-ref tokens 0))) 
              (datum->syntax name
                (string->symbol 
                  (string-append (vector-ref tokens 1) "?")))
              (vector-ref tokens 1)))))
    (define (make-guard ai)
      (with-syntax ((n (car ai))
                    (g (cadr ai))
                    (l loc)
                    (s (string-append "not " (cddr ai))))
        #'(unless (g n)
            (assertion-violation 'l s n))))
    (define (make-list-guard ai)
      (with-syntax ((n (car ai))
                    (g (cadr ai))
                    (l loc)
                    (s (string-append "not " (cddr ai))))
        ; the compiler generates stupid code for this, but still faster than using for-all
        #'(let loop ((n n))
               (unless (null? n)
                 (unless (g ($car n)) (assertion-violation 'l s ($car n)))
                 (loop ($cdr n))))))
    (syntax-case x ()
      [((a ...) body body* ...)
        (for-all identifier? #'(a ...))
        (let ((ai (map get-name/type #'(a ...))))
          (with-syntax (((a ...) (map car ai))
                        ((g ...) (map make-guard (filter cdr ai)))) 
            #'((a ...)
                g ...
                (let ((a a) ...) body body* ...))))]
      [((a a* ... . rest) body body* ...)                
        (and (for-all identifier? #'(a a* ...)) (identifier? #'rest))
        (let ((ai (map get-name/type #'(a a* ...)))
              (ri (get-name/type #'rest)))
          (with-syntax (((a ...) (map car ai))
                        ((g ...) (map make-guard (filter cdr ai)))
                        (rest (car ri))
                        (h (if (cdr ri) (make-list-guard ri) #'#f))) 
            #'((a ... . rest)
                g ...
                h
                (let ((a a) ... (rest rest)) body body* ...))))]
      [(formals body body* ...)
        (identifier? #'formals)                
        (let ((ri (get-name/type #'formals)))
          (with-syntax ((formals (car ri))
                        (h (if (cdr ri) (make-list-guard ri) #'#f))) 
            #'(formals
                h
                (let ((formals formals)) body body* ...))))])))
