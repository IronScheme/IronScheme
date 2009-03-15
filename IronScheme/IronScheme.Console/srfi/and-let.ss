(library (srfi and-let)
  ;; can't be named (srfi and-let*) because some OS's filenames can't have *
  (export and-let*)
  (import (rnrs))

  (define-syntax and-let*
    (lambda (stx)

      (define (unique-ids? ls)
        (or (null? ls)
            (and (let notmem? ([x (car ls)] [ls (cdr ls)])
                   (or (null? ls)
                       (and (not (and (bound-identifier=? x (car ls))
                                      (syntax-violation #f "duplicate binding" stx x)))
                            (notmem? x (cdr ls)))))
                 (unique-ids? (cdr ls)))))

      (define (get-and-check-ids clauses)
        (let loop ([c* clauses])
          (syntax-case c* ()
            [([var expr] . rest)
             (or (identifier? #'var)
                 (syntax-violation #f "not an identifier" stx #'var))
             (cons #'var (loop #'rest))]
            [(whatever . rest)
             (loop #'rest)]
            [()
             '()])))

      ;;; First check these
      (syntax-case stx ()
        [(_ (clause* ...) body* ...)
         (unique-ids? (get-and-check-ids #'(clause* ...)))
         'ignore])

      ;;; Then construct output syntax
      (syntax-case stx ()
        [(_ (clause* ...) body* ...)
         #'(and-let*-core #t (clause* ...) body* ...)])))

  (define-syntax and-let*-core
    (lambda (stx)
      (syntax-case stx ()
        [(kw ignore ([var expr] clause* ...) body* ...)
         #'(let ([var expr])
             (if var
               (kw var (clause* ...) body* ...)
               #f))]
        [(kw ignore ([expr] clause* ...) body* ...) ;;; <=== This one ***
         (with-syntax ([(g) (generate-temporaries '(var))])
           #'(let ([g expr])
               (if g
                 (kw g (clause* ...) body* ...)
                 #f)))]
        [(kw ignore (id clause* ...) body* ...)
         (or (identifier? #'id)
             (syntax-violation #f "invalid clause" stx #'id))
         #'(if id
             (kw id (clause* ...) body* ...)
             #f)]
        [(kw last () body* ...)
         (if (positive? (length #'(body* ...)))
           #'(begin body* ...)
           #'last)])))
)