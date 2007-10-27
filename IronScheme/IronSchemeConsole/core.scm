;;; derived forms, fully functional, but no error checking
;;; these are hygienic too, and optimized when possible

;; let is first as we use it everywhere
;; note: you cant use let in the body, but it's ok in the output
(define let
  (macro (args . body)
         ;; check named let
         (if (symbol? args)
             ((lambda (name args body)
                `(let ((,name #f))
                   (set! ,name (lambda ,(map first args) ,@body))
                   (,name ,@(map second args))))
              args (car body) (cdr body))
             ;; normal let
             `((lambda ,(map first args) ,@body) ,@(map second args)))))

;; marco helper
(define define-macro
  (macro (nargs . body)
         (let ((name (car nargs))
               (args (cdr nargs)))
           `(define ,name (macro ,args ,@body)))))

;; most basic form
(define-macro (begin . e)
  ;; check for single term, no closure needed
  (if (null? (cdr e))
      (car e)
      `((lambda () ,@e))))

;; let* in terms of itself and let 
(define-macro (let* args . body)
  (if (null? (cdr args))
      `(let ,args ,@body)
      `(let (,(car args))
         (let* ,(cdr args) ,@body))))
         
(define (void) (if #f #f))


;; beast #1: if .. elseif ... else
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

(define-macro (and . e)
  (cond
   ;; empty
   ((null? e) #t)
   ;; single
   ((null? (cdr e)) (car e))
   (else
    `(if ,(car e)
         (and ,@(cdr e))
         #f))))


(define-macro (or . e)
  (cond
   ;; empty
   ((null? e) #f)
   ;; single
   ((null? (cdr e)) (car e))
   (else
    (let ((t (gensym)))
      `(let ((,t ,(car e)))
         (if ,t ,t
             (or ,@(cdr e))))))))

;; chest hair grower #2: same as cond, but has a test before the clauses
(define-macro (case test . clauses)
  ;; helper to take care of recursion after the tests
  (define (case-helper test . clauses)
      (if(null? clauses) `(if #f #f)
          (let ((clause (car clauses))
                (rest (cdr clauses))
                (t (gensym)))
            (if (eq? (car clause) 'else)
                `(begin ,@(cdr clause))
                `(let ((,t ',(car clause)))
                   (if (memv ,test ,t)
                       (begin ,@(cdr clause))
                       ;; dont call case, else extra variables are inserted
                       ,(apply case-helper test rest)))))))
  (let ((t (gensym)))
    `(let ((,t ,test))
       ,(apply case-helper t clauses))))

;; not as hairy as I thought, maybe I am just getting better :)
(define-macro (do clauses test . cmds)
  ;; helper for inits
  (define (getinit clause)
      (list (car clause) (second clause)))

  ;; helper for successive calls
  (define (getnext clause)
      (let ((second (cdr clause)))
        (if (null? (cdr second))
            (car clause)
            (third clause))))

  ;; lets begin :)
  (let ((t (gensym)))
    `(let ,t ,(map getinit clauses)
       (if ,(car test)
           (begin ,@(cdr test))
           (begin ,@cmds (,t ,@(map getnext clauses)))))))
           
 ;; now its getting easy :)
 (define-macro (letrec args . body)
   ;; init to values inside body
   (define (init-helper temp args)
     `(set! ,(car args) ,temp))

   ;; helper to init vars to false
   (define (false a) #f)

   (define temps (map (lambda (a)(gensym)) args))

   ;; lets begin
   `((lambda ,(map car args)
       ((lambda ,temps
         ,@(map init-helper temps args)
         ,@body)
       ,@(map second args)))
     ,@(map false args)))         


;; now its getting easy :)
(define-macro (letrec* args . body)
  ;; init to values inside body
  (define (init-helper args)
      `(set! ,(car args) ,(second args)))
  
  ;; helper to init vars to false
  (define (false a) #f)
  
  ;; lets begin
  `((lambda ,(map car args)
      ,@(map init-helper args)
      ,@body) ,@(map false args)))




