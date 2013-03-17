#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme clr shorthand-helper)
  (export 
    lit=?
    valid-clr-name?
    parse-clause)
  (import 
    (ironscheme)
    (ironscheme clr))
  
  (define (lit=? id sym)
    (eq? (syntax->datum id) sym))
    
  (define (valid-clr-name? sym)
    (and (identifier? sym)
         (let ((s (string->list (symbol->string (syntax->datum sym)))))
           (for-all 
            (lambda (c)
              (or (char-alphabetic? c)
                  (char=? c #\_)
                  (char-numeric? c)))
            s))))
            
  (define (parse-clause id type)
    (with-syntax ((id id)(type type))
      #'(lambda (x)
          (syntax-case x ()
            [(_ . rest)
              (let f ((x* #'rest)(id #'id)(type #'type))
                (with-syntax ((id id)(type type))
                  (syntax-case x* () 
                    [(: meth (arg (... ...)))
                      (and (lit=? #': ':) 
                           (or (valid-clr-name? #'meth) 
                               (string? (syntax->datum #'meth))))
                      #'(clr-call type meth id arg (... ...))]
                    [(: meth (arg (... ...)) . rest)
                      (and (lit=? #': ':) 
                           (or (valid-clr-name? #'meth) 
                               (string? (syntax->datum #'meth))))
                      (f #'rest #'(clr-call type meth id arg (... ...)) #'#f)]   
                    [(: prop = value)
                      (and (lit=? #': ':) (lit=? #'= '=) (valid-clr-name? #'prop))
                      #'(clr-prop-set! type prop id value)]
                    [(: prop)
                      (and (lit=? #': ':) (valid-clr-name? #'prop))
                      #'(clr-prop-get type prop id)]
                    [(: prop . rest)
                      (and (lit=? #': ':) (valid-clr-name? #'prop))
                      (f #'rest #'(clr-prop-get type prop id) #'#f)]
                    [(-> field = value)
                      (and (lit=? #'-> '->) (lit=? #'= '=) (valid-clr-name? #'field))
                      #'(clr-field-set! type field id value)]
                    [(-> field)
                      (and (lit=? #'-> '->) (valid-clr-name? #'field))
                      #'(clr-field-get type field id)]
                    [(-> field . rest)
                      (and (lit=? #'-> '->) (valid-clr-name? #'field))
                      (f #'rest #'(clr-field-get type field id) #'#f)]
                    [(: (arg arg* (... ...)) = value)
                      (and (lit=? #': ':) (lit=? #'= '=))
                      #'(clr-indexer-set! type id arg arg* (... ...) value)]
                    [(: (arg arg* (... ...)))
                      (lit=? #': ':)
                      #'(clr-indexer-get type id arg arg* (... ...))]
                    [(: (arg arg* (... ...)) . rest)
                      (lit=? #': ':)
                      (f #'rest #'(clr-indexer-get type id arg arg* (... ...)) #'#f)]
                    [(args (... ...)) 
                      (syntax-violation 'with-clr-type "invalid syntax" x x*)])))]
              [_ #'id])))))