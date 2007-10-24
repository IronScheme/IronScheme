
(define let
  (macro (args . body)
    (if (symbol? args)
        ;; cant use let here yet :(
        ((lambda (name args body)
           `(let ((,name #f))
              (set! ,name (lambda ,(map first args) ,@body))
              (,name ,@(map second args))))
         args (car body) (cdr body))
        `((lambda ,(map first args) ,@body) ,@(map second args)))))

(define define-macro
  (macro (nargs . body)
    (let ((name (car nargs))
          (args (cdr nargs)))
      `(define ,name (macro ,args ,@body)))))

(define-macro (begin . e)
  `((lambda () ,@e)))

(define-macro (let* args . body)
  (if (null? (cdr args))
      `(let ,args ,@body)
      `(let (,(car args))
         (let* ,(cdr args) ,@body))))

(define-macro (and . e)
  (if (null? e) #t
      (if (null? (cdr e)) (car e)
          `(if ,(car e)
               (and ,@(cdr e))
               #f))))

(define-macro (or . e)
  (if (null? e) #f
      (if (null? (cdr e)) (car e)
          (let ((t (gensym)))
            `(let ((,t ,(car e)))
               (if ,t ,t
                   (or ,@(cdr e))))))))
                   
                   
(define-macro (cond . clauses)
  (if (null? clauses)
      ;; return unspecified
      `(if #f #f)
      ;; look at first clause
      (let ((clause (car clauses))
            (rest (cdr clauses))
            (t (gensym)))
        ;; single case 
        (if (null? (cdr clause))
            `(let ((,t ,(car clause)))
               (if ,t ,t
                   (cond ,@rest)))
            ;; check for else
            (if (eq? (car clause) 'else)
                `(begin ,@(cdr clause))
                ;; check for =>
                (if (eq? (second clause) '=>)
                    `(let ((,t ,(car clause)))
                       (if ,t (,(third clause) ,t)
                           (cond ,@rest)))
                    ;; last case
                    `(let ((,t ,(car clause)))
                       (if ,t (begin ,@(cdr clause))
                           (cond ,@rest)))))))))
                   