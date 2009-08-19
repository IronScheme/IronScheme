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

(library (ironscheme clr)
  (export
    clr-static-event-add!
    clr-static-event-remove!
    clr-event-add!
    clr-event-remove!
    clr-using
    clr-reference
    clr-is
    clr-cast
    clr-call
    clr-static-call
    clr-field-get
    clr-field-set!
    clr-static-field-get
    clr-static-field-set!
    clr-prop-get
    clr-prop-set!
    clr-static-prop-get
    clr-static-prop-set!
    clr-indexer-get
    clr-indexer-set!
    clr-new
    clr-new-array
    pinvoke-call
    with-clr-type
    let-clr-type
    )
  (import
    (rnrs)
    (ironscheme clr helpers)
    (ironscheme clr internal))
        
  (define-syntax pinvoke-call
    (lambda (x)
      (define (->string id) 
        (symbol->string (syntax->datum id)))
      (syntax-case x ()
        [(_ lib proc ret (args ...))
          (with-syntax (((args ...) (map ->string #'(lib proc ret args ...))))
            #'(pinvoke-call-internal args ...))])))
            

  (define-syntax clr-using
    (lambda (e)
      (syntax-case e ()
        [(_ namespace)
         #`(define using (clr-using-internal 'namespace))])))

  (define-syntax clr-reference
    (lambda (e)
      (syntax-case e ()
        [(_ assname)
         #`(define reference (clr-reference-internal 'assname))])))

  (define-syntax clr-is
    (lambda (e)
      (syntax-case e ()
        [(_ type arg)
         #'(clr-is-internal 'type arg)])))

  (define-syntax clr-call
    (lambda (e)
      (syntax-case e ()
        [(_ type member instance args ...)
         #'(clr-call-internal 'type 'member instance args ...)])))

  (define-syntax clr-field-get
    (lambda (e)
      (syntax-case e ()
        [(_ type member instance)
         #'(clr-field-get-internal 'type 'member instance)])))

  (define-syntax clr-field-set!
    (lambda (e)
      (syntax-case e ()
        [(_ type member instance value)
         #'(clr-field-set!-internal 'type 'member instance value)])))

  (define-syntax clr-static-field-get
    (syntax-rules ()
      [(_ type member)
       (clr-field-get type member '())]))

  (define-syntax clr-static-field-set!
    (syntax-rules ()
      [(_ type member value)
       (clr-field-set! type member '() value)]))

  (define-syntax clr-static-call
    (syntax-rules ()
      [(_ type member args ...)
       (clr-call type member '() args ...)]))

  (define-syntax clr-static-event-add!
    (syntax-rules ()
      [(_ type event handler)
       (clr-event-add! type event '() handler)]))

  (define-syntax clr-static-event-remove!
    (syntax-rules ()
      [(_ type event handler)
       (clr-event-remove! type event '() handler)]))
       
  (define-syntax clr-event-add!
    (lambda (e)
      (syntax-case e ()
        [(_ type event-name instance handler)
         #`(clr-call-internal
            'type
            '#,(prefix-syntax "add_" #'event-name)
            instance handler)])))

  (define-syntax clr-event-remove!
    (lambda (e)
      (syntax-case e ()
        [(_ type event-name instance handler)
         #`(clr-call-internal
            'type
            '#,(prefix-syntax "remove_" #'event-name)
            instance handler)])))

  (define-syntax clr-prop-get
    (lambda (e)
      (syntax-case e ()
        [(_ type prop-name instance args ...)
         #`(clr-call-internal
            'type
            '#,(prefix-syntax "get_" #'prop-name)
            instance args ...)])))

  (define-syntax clr-prop-set!
    (lambda (e)
      (syntax-case e ()
        [(_ type prop-name instance args ...)
         #`(clr-call-internal
            'type
            '#,(prefix-syntax "set_" #'prop-name)
            instance args ...)])))

  (define-syntax clr-indexer-get
    (syntax-rules ()
      [(_ type instance arg args* ...)
       (clr-prop-get type item instance arg args* ...)]))

  (define-syntax clr-indexer-set!
    (syntax-rules ()
      [(_ type instance arg args* ... value)
       (clr-prop-set! type item instance arg args* ... value)]))

  (define-syntax clr-static-prop-get
    (syntax-rules ()
      [(_ type prop-name args ...)
       (clr-prop-get type prop-name '() args ...)]))

  (define-syntax clr-static-prop-set!
    (syntax-rules ()
      [(_ type prop-name args ...)
       (clr-prop-set! type prop-name '() args ...)]))

  (define-syntax clr-new
    (lambda (e)
      (syntax-case e ()
        [(_ type args ...)
         #'(clr-new-internal 'type args ...)])))

  (define-syntax clr-new-array
    (lambda (e)
      (syntax-case e ()
        [(_ type size)
         #'(clr-new-array-internal 'type size)])))

  (define-syntax clr-cast
    (lambda (e)
      (syntax-case e ()
        [(_ type arg)
         #'(clr-cast-internal 'type arg)])))

  (define (lit=? id sym)
    (eq? (syntax->datum id) sym))

  (define-syntax let-clr-type 
    (lambda (x)
      (syntax-case x ()
        [(_ ((id (type arg ...)) ...) b b* ...)
          (for-all identifier? #'(id ... type ...))
          #'(let ((id (clr-new type arg ...)) ...)
              (with-clr-type ((id type) ...)
                b b* ...))])))         

  (define-syntax with-clr-type
    (lambda (x)
      (define (parse id type)
        (with-syntax ((id id)(type type))
          #'(id 
              (lambda (x)
                (syntax-case x () 
                  [(_ : prop = value)
                    (and (identifier? #'prop) (lit=? #': ':) (lit=? #'= '=))
                    #'(clr-prop-set! type prop id value)]
                  [(_ : prop)
                    (and (identifier? #'prop) (lit=? #': ':))
                    #'(clr-prop-get type prop id)]
                  [(_ -> field = value)
                    (and (identifier? #'field) (lit=? #'-> '->) (lit=? #'= '=))
                    #'(clr-field-set! type field id value)]
                  [(_ -> field)
                    (and (identifier? #'field) (lit=? #'-> '->))
                    #'(clr-field-get type field id)]
                  [(_ meth . arg)
                    (or (identifier? #'meth) (string? (syntax->datum #'meth)))
                    #'(clr-call type meth id . arg)]
                  [(_ (arg arg* (... ...)) = value)
                    (lit=? #'= '=)
                    #'(clr-indexer-set! type id arg arg* (... ...) value)]
                  [(_ (arg arg* (... ...)))
                    #'(clr-indexer-get type id arg arg* (... ...))]
                  [(_ . args) 
                    (syntax-violation 'with-clr-type "invalid syntax" x #f)]
                  [_ #'id])))))
      (syntax-case x ()
        [(_ ((id type) ...) b b* ...)
          (for-all identifier? #'(id ... type ...))
          (with-syntax (((e ...) (map parse #'(id ...) #'(type ...))))
            #'(let-syntax (e ...) b b* ...))])))


  )
