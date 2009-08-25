#| ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 2007,2008,2009
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************|#

(library (ironscheme docs topic-printer)
  (export 
    print-topic)
  (import
    (ironscheme)
    (ironscheme console)
    (ironscheme docs topic))
    
  (define (print-color color fmt . args)
    (let ((c (foreground-color)))
      (dynamic-wind
        (lambda () (foreground-color color))
        (lambda () (apply printf fmt args))
        (lambda () (foreground-color c)))))
    
  (define (print-title title)
    (print-color 'cyan " ~a:\n" title))    
    
  (define (print-body body)
    (printf " ~a\n\n" body))
    
  (define (print-name name)
    (print-color 'green " ~a\n\n" name))
    
  (define (print-description description)
    (when description 
      (printf " ~a\n\n" description)))
    
  (define (print-form form)
    (print-title "Form")
    (print-body form))
    
  (define (print-param p)
    (print-color 'yellow (string-format " {0,-15}" (car p)))
    (printf "~a\n" (cadr p)))

  (define (print-params params)
    (print-title "Parameters")
    (if (null? params)
      (print-body "None")
      (begin
        (for-each print-param params)
        (newline))))

  (define (print-return return)
    (print-title "Return")
    (print-body return))

  (define (print-exception e)
    (print-color 'red (string-format " {0,-15}" (car e)))
    (printf "~a\n" (cadr e)))

  (define (print-exceptions exceptions)
    (unless (null? exceptions)
      (print-title "Exceptions")
      (for-each print-exception exceptions)
      (newline)))

  (define (print-remark remark)
    (when remark
      (print-title "Remarks")
      (print-body remark)))
  
  (define (print-code c)
    (print-color 'green " ~a\n" (strip (format "~a" c))))
  
  (define (strip c)
    (substring c 1 (fx- (string-length c) 1)))

  (define (print-example e)
    (if (string? (car e))
      (begin
        (printf " ~a\n" (car e))
        (print-code (cdr e)))
      (print-code e)))

  (define (print-examples examples)
    (unless (null? examples)
      (print-title "Examples")
      (for-each print-example examples)
      (newline)))

  (define (print-library library)
    (print-title "Library")
    (for-each 
      (lambda (library)
        (print-color 'green " ~a\n" library))
      library)
    (newline))

  (define (print-related related)
    (unless (null? related)
      (print-title "Related")
      (for-each (lambda (r) (print-color 'green " ~a\n" r)) related)))

  (define (print-topic t)
    (assert (topic? t))    
    (print-name (topic-name t))
    (print-description (topic-description t))
    (print-form (topic-form t))
    (print-params (topic-params t))
    (print-return (topic-return t))
    (print-exceptions (topic-exceptions t))
    (print-remark (topic-remark t))
    (print-examples (topic-examples t))
    (print-library (topic-library t))
    (print-related (topic-related t)))
     
)
    
