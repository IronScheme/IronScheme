 
   (define make-parameter
      (lambda (init . conv)
        (let ((converter
               (if (null? conv) (lambda (x) x) (car conv))))
          (let ((global-cell
                 (cons #f (converter init))))
            (letrec ((parameter
                      (lambda new-val
                        (let ((cell (dynamic-lookup parameter global-cell)))
                          (cond ((null? new-val)
                                 (cdr cell))
                                ((null? (cdr new-val))
                                 (set-cdr! cell (converter (car new-val))))
                                (else ; this case is needed for parameterize
                                 (converter (car new-val))))))))
              (set-car! global-cell parameter)
              parameter)))))

    (define-syntax parameterize
      (syntax-rules ()
        ((parameterize ((expr1 expr2) ...) body ...)
         (dynamic-bind (list expr1 ...)
                       (list expr2 ...)
                       (lambda () body ...)))))

    (define dynamic-bind
      (lambda (parameters values body)
        (let* ((old-local
                (dynamic-env-local-get))
               (new-cells
                (map (lambda (parameter value)
                       (cons parameter (parameter value #f)))
                     parameters
                     values))
               (new-local
                (append new-cells old-local)))
          (dynamic-wind
            (lambda () (dynamic-env-local-set! new-local))
            body
            (lambda () (dynamic-env-local-set! old-local))))))

    (define dynamic-lookup
      (lambda (parameter global-cell)
        (or (assq parameter (dynamic-env-local-get))
            global-cell)))

    (define dynamic-env-local '())

    (define dynamic-env-local-get
      (lambda () dynamic-env-local))

    (define dynamic-env-local-set!
      (lambda (new-env) (set! dynamic-env-local new-env)))
