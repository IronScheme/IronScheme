;------------------------------------------------------------------------------

; Macro expander for define*.

(define-macro (define* pattern . body)
  (if (pair? pattern)
      `(define ,(car pattern)
         (lambda* ,(cdr pattern) ,@body))
      `(define ,pattern ,@body)))

; Macro expander for lambda*.

(define-macro (lambda* formals . body)

;------------------------------------------------------------------------------

; Procedures needed at expansion time.

(define (parse-formals formals)

  (define (variable? x) (symbol? x))

  (define (required-positional? x)
    (variable? x))

  (define (optional-positional? x)
    (and (pair? x)
         (pair? (cdr x))
         (null? (cddr x))
         (variable? (car x))))

  (define (required-named? x)
    (and (pair? x)
         (pair? (cdr x))
         (null? (cddr x))
         (keyword? (car x))
         (variable? (cadr x))))

  (define (optional-named? x)
    (and (pair? x)
         (pair? (cdr x))
         (pair? (cddr x))
         (null? (cdddr x))
         (keyword? (car x))
         (variable? (cadr x))))

  (define (named? x)
    (or (required-named? x)
        (optional-named? x)))

  (define (duplicates? lst)
    (cond ((null? lst)
           #f)
          ((memq (car lst) (cdr lst))
           #t)
          (else
           (duplicates? (cdr lst)))))

  (define (parse-positional-section lst cont)
    (let loop1 ((lst lst) (rev-reqs '()))
      (if (and (pair? lst)
               (required-positional? (car lst)))
          (loop1 (cdr lst) (cons (car lst) rev-reqs))
          (let loop2 ((lst lst) (rev-opts '()))
            (if (and (pair? lst)
                     (optional-positional? (car lst)))
                (loop2 (cdr lst) (cons (car lst) rev-opts))
                (cont lst (cons (reverse rev-reqs) (reverse rev-opts))))))))

  (define (parse-named-section lst cont)
    (let loop ((lst lst) (rev-named '()))
      (if (and (pair? lst)
               (named? (car lst)))
          (loop (cdr lst) (cons (car lst) rev-named))
          (cont lst (reverse rev-named)))))

  (define (parse-rest lst
                      positional-before-named?
                      positional-reqs/opts
                      named)
    (if (null? lst)
        (parse-end positional-before-named?
                   positional-reqs/opts
                   named
                   #f)
        (if (variable? lst)
            (parse-end positional-before-named?
                       positional-reqs/opts
                       named
                       lst)
            (error "syntax error in formal parameter list"))))

  (define (parse-end positional-before-named?
                     positional-reqs/opts
                     named
                     rest)
    (let ((positional-reqs (car positional-reqs/opts))
          (positional-opts (cdr positional-reqs/opts)))
      (let ((vars
             (append positional-reqs
                     (map car positional-opts)
                     (map cadr named)
                     (if rest (list rest) '())))
            (keys
             (map car named)))
        (cond ((duplicates? vars)
               (error "duplicate variable in formal parameter list"))
              ((duplicates? keys)
               (error "duplicate keyword in formal parameter list"))
              (else
               (list positional-before-named?
                     positional-reqs
                     positional-opts
                     named
                     rest))))))

  (define (parse lst)
    (if (and (pair? lst)
             (named? (car lst)))
        (parse-named-section
         lst
         (lambda (lst named)
           (parse-positional-section
            lst
            (lambda (lst positional-reqs/opts)
              (parse-rest lst
                          #f
                          positional-reqs/opts
                          named)))))
        (parse-positional-section
         lst
         (lambda (lst positional-reqs/opts)
           (parse-named-section
            lst
            (lambda (lst named)
              (parse-rest lst
                          #t
                          positional-reqs/opts
                          named)))))))

  (parse formals))

(define (expand-lambda* formals body)

  (define (range lo hi)
    (if (< lo hi)
        (cons lo (range (+ lo 1) hi))
        '()))

  (define (expand positional-before-named?
                  positional-reqs
                  positional-opts
                  named
                  rest)
    (if (and (null? positional-opts) (null? named)) ; direct R5RS equivalent

        `(lambda ,(append positional-reqs (or rest '())) ,@body)

        (let ()

          (define utility-fns
            `(,@(if (or positional-before-named?
                        (null? positional-reqs))
                    `()
                    `(($req
                       (lambda ()
                         (if (pair? $args)
                             (let ((arg (car $args)))
                               (set! $args (cdr $args))
                               arg)
                             (error "too few actual parameters"))))))
              ,@(if (null? positional-opts)
                    `()
                    `(($opt
                       (lambda (default)
                         (if (pair? $args)
                             (let ((arg (car $args)))
                               (set! $args (cdr $args))
                               arg)
                             (default))))))))

          (define positional-bindings
            `(,@(if positional-before-named?
                    `()
                    (map (lambda (x)
                           `(,x ($req)))
                         positional-reqs))
              ,@(map (lambda (x)
                       `(,(car x) ($opt (lambda () ,(cadr x)))))
                     positional-opts)))

          (define named-bindings
            (if (null? named)
                `()
                `(($key-values
                   (vector ,@(map (lambda (x) `$undefined)
                                  named)))
                  ($args
                   ($process-keys
                    $args
                    ',(make-perfect-hash-table
                       (map (lambda (x i)
                              (cons (car x) i))
                            named
                            (range 0 (length named))))
                    $key-values))
                  ,@(map (lambda (x i)
                           `(,(cadr x)
                             ,(if (null? (cddr x))
                                  `($req-key $key-values ,i)
                                  `($opt-key $key-values ,i (lambda ()
                                                              ,(caddr x))))))
                         named
                         (range 0 (length named))))))

          (define rest-binding
            (if (not rest)
                `(($args (or (null? $args)
                             (error "too many actual parameters"))))
                `((,rest $args))))

          (let ((bindings
                 (append (if positional-before-named?
                             (append utility-fns
                                     positional-bindings
                                     named-bindings)
                             (append named-bindings
                                     utility-fns
                                     positional-bindings))
                         rest-binding)))
            `(lambda ,(append (if positional-before-named?
                                  positional-reqs
                                  '())
                              '$args)
               (let* ,bindings
                 ,@body))))))

  (apply expand (parse-formals formals)))

(define (make-perfect-hash-table alist)

  ; "alist" is a list of pairs of the form "(keyword . value)"

  ; The result is a perfect hash-table represented as a vector of
  ; length 2*N, where N is the hash modulus.  If the keyword K is in
  ; the hash-table it is at index
  ;
  ;   X = (* 2 ($hash-keyword K N))
  ;
  ; and the associated value is at index X+1.

  (let loop1 ((n (length alist)))
    (let ((v (make-vector (* 2 n) #f)))
      (let loop2 ((lst alist))
        (if (pair? lst)
            (let* ((key-val (car lst))
                   (key (car key-val)))
              (let ((x (* 2 ($hash-keyword key n))))
                (if (vector-ref v x)
                    (loop1 (+ n 1))
                    (begin
                      (vector-set! v x key)
                      (vector-set! v (+ x 1) (cdr key-val))
                      (loop2 (cdr lst))))))
            v)))))

(define ($hash-keyword key n)
  (let ((str (keyword->string key)))
    (let loop ((h 0) (i 0))
      (if (< i (string-length str))
          (loop (modulo (+ (* h 65536) (char->integer (string-ref str i)))
                        n)
                (+ i 1))
          h))))

(expand-lambda* formals body))

;------------------------------------------------------------------------------

; Procedures needed at run time (called by the expanded code):

; Perfect hash-tables with keyword keys.

(define ($hash-keyword key n)
  (let ((str (keyword->string key)))
    (let loop ((h 0) (i 0))
      (if (< i (string-length str))
          (loop (modulo (+ (* h 65536) (char->integer (string-ref str i)))
                        n)
                (+ i 1))
          h))))

(define ($perfect-hash-table-lookup table key)
  (let* ((n (quotient (vector-length table) 2))
         (x (* 2 ($hash-keyword key n))))
    (and (eq? (vector-ref table x) key)
         (vector-ref table (+ x 1)))))

; Handling of named parameters.

(define $undefined (list 'undefined))

(define ($req-key key-values i)
  (let ((val (vector-ref key-values i)))
    (if (eq? val $undefined)
        (error "a required named parameter was not provided")
        val)))

(define ($opt-key key-values i default)
  (let ((val (vector-ref key-values i)))
    (if (eq? val $undefined)
        (default)
        val)))

(define ($process-keys args key-hash-table key-values)
  (let loop ((args args))
    (if (null? args)
        args
        (let ((k (car args)))
          (if (not (keyword? k))
              args
              (let ((i ($perfect-hash-table-lookup key-hash-table k)))
                (if (not i)
                    (error "unknown parameter keyword" k)
                    (if (null? (cdr args))
                        (error "a value was expected after keyword" k)
                        (begin
                          (if (eq? (vector-ref key-values i) $undefined)
                              (vector-set! key-values i (cadr args))
                              (error "duplicate parameter" k))
                          (loop (cddr args)))))))))))

;------------------------------------------------------------------------------

