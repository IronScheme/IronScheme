;============================================================================================
; IMPLEMENTATION:
;
; Andre van Tonder, 2004.
;
;============================================================================================

(define-syntax define-record-type    
  (syntax-rules ()
    ((define-record-type . body)
     (parse-declaration #f . body))))

(define-syntax define-record-scheme    
  (syntax-rules ()
    ((define-record-scheme . body)
     (parse-declaration #t . body))))

(define-syntax parse-declaration    
  (syntax-rules ()
    ((parse-declaration is-scheme? (name super ...) constructor-clause predicate field-clause ...)
     (build-record 0 constructor-clause (super ...) (field-clause ...) name predicate is-scheme?))
    ((parse-declaration is-scheme? (name super ...) constructor-clause)
     (parse-declaration is-scheme? (name super ...) constructor-clause #f))  
    ((parse-declaration is-scheme? (name super ...))
     (parse-declaration is-scheme? (name super ...) #f #f))
    ((parse-declaration is-scheme? name . rest)
     (parse-declaration is-scheme? (name) . rest))))

(define-syntax record-update!
  (syntax-rules ()
    ((record-update! record name (label exp) ...)
     (meta
      `(let ((r record)) 
         ((meta ,(name ("setter") label)) r exp)
         ...
         r)))))

(define-syntax record-update
  (syntax-rules ()
    ((record-update record name (label exp) ...)
     (name ("is-scheme?")
           (meta                                                         
            `(let ((new ((meta ,(name ("copier"))) record)))
               (record-update! new name (label exp) ...)))
           (record-compose (name record) (name (label exp) ...))))))    
           
(define-syntax record-compose
  (syntax-rules ()
    ((record-compose (export-name (label exp) ...))
     (export-name (label exp) ...))
    ((record-compose (import-name record) ... (export-name (label exp) ...))
     (help-compose 1 (import-name record) ... (export-name (label exp) ...)))))

(define-syntax help-compose
  (syntax-rules ()
    ((help-compose 1 (import-name record) import ... (export-name (label exp) ...))
     (meta
      `(help-compose 2
                     (meta ,(intersection
                             (meta ,(export-name ("labels")))
                             (meta ,(remove-from (meta ,(import-name ("labels")))
                                                 (label ...)
                                                 if-free=))
                             if-free=))
                     (import-name record) 
                     import ...
                     (export-name (label exp) ...))))
    ((help-compose 2 (copy-label ...) (import-name record) import ... (export-name . bindings))
     (meta
      `(let ((r record))
         (record-compose import ...
           (export-name (copy-label ((meta ,(import-name ("getter") copy-label)) r))
                        ...
                        . bindings)))))))

(define-syntax build-record
  (syntax-rules ()
   ((build-record 0 (constructor . pos-labels) . rest)              ; extract positional labels from constructor clause
    (build-record 1 (constructor . pos-labels) pos-labels . rest))  ; 
   ((build-record 0 constructor . rest)                             ; 
    (build-record 1 (constructor . #f) () . rest))                  ; 
   ((build-record 1 constructor-clause (pos-label ...) (super ...)  
                    ((label . accessors) ...) . rest)
    (meta 
     `(build-record 2
                    constructor-clause
                    (meta ,(union (meta ,(super ("labels")))        ; compute union of labels from supers,
                                  ...                               ; constructor clause and field clauses
                                  (pos-label ...) 
                                  (label ...)      
                                  top:if-free=))
                    ((label . accessors) ...)
                    (meta  ,(union (meta ,(super ("supers")))       ; compute transitive union of supers
                                   ...
                                   top:if-free=))
                    . rest)))
    ((build-record 2 (constructor . pos-labels) labels . rest)      ; insert default constructor labels if not given
     (syntax-if pos-labels
                (build-record 3 (constructor . pos-labels) labels . rest)
                (build-record 3 (constructor . labels)     labels . rest)))
    ((build-record 3 constructor-clause labels ((label . accessors) ...) . rest)
     (meta 
      `(build-record 4
                     (meta ,(remove-from labels                     ; separate the labels that do not appear in a
                                         (label ...)                ; field clause for next step
                                         top:if-free=))
                     ((label . accessors) ...) 
                     constructor-clause
                     labels
                     . rest)))
    ((build-record 4
                   (undeclared-label ...)
                   (field-clause ...)
                   (constructor . pos-labels)
                   labels
                   supers
                   name
                   predicate
                   is-scheme?)
     (meta
      `(build-record 5                                              ; generate identifiers for constructor, predicate
                     is-scheme?                                     ; getters and setters as needed 
                     name
                     supers
                     supers
                     labels 
                     (meta ,(to-identifier constructor))   
                     (meta ,(add-temporaries pos-labels))           ; needed for constructor below
                     (meta ,(to-identifier predicate))
                     (meta ,(augment-field field-clause)) 
                     ... 
                     (undeclared-label (meta ,(generate-identifier))
                                       (meta ,(generate-identifier)))
                     ...)))
    ((build-record 5
                   is-scheme?
                   name
                   (super ...)
                   supers
                   (label ...)
                   constructor  
                   ((pos-label pos-temp) ...) 
                   predicate
                   (field-label getter setter)
                   ...)  
     
     (begin
       (syntax-if is-scheme?
                  
                  (begin
                    (define-generic (predicate x) (lambda (x) #f))
                    (define-generic (getter x))
                    ...
                    (define-generic (setter x v))
                    ...
                    (define-generic (copy x)))
                  
                  (begin
                    (srfi-9:define-record-type internal-name
                                               (maker field-label ...)
                                               predicate
                                               (field-label getter setter) ...)  
       
                    (define constructor 
                      (lambda (pos-temp ...)
                        (populate 1 maker (field-label ...) (pos-label pos-temp) ...)))
       
                    (extend-predicates supers predicate)
                    (extend-accessors supers field-label predicate getter setter)
                    ...
       
                    (define (copy x)
                      (maker (getter x) ...))
                    (extend-copiers supers copy predicate)
   
                    (define-method (show (r predicate))
                      (list 'name
                            (list 'field-label (getter r)) 
                            ...))))    
       
       (define-syntax name
         (syntax-rules (field-label ...)
           ((name ("is-scheme?") sk fk)     (syntax-if is-scheme? sk fk))
           ((name ("predicate") k)          (syntax-apply k predicate))
           ((name ("supers") k)             (syntax-apply k (super ... name)))  
           ((name ("labels") k)             (syntax-apply k (label ...)))
           ((name ("pos-labels") k)         (syntax-apply k (pos-label ...)))
           ((name ("getter") field-label k) (syntax-apply k getter))   
           ...
           ((name ("getter") other k)       (syntax-apply k #f))
           ((name ("setter") field-label k) (syntax-apply k setter))  
           ...
           ((name ("setter") other k)       (syntax-apply k #f))
           ((name ("copier") k)             (syntax-apply k copy))
           ((name . bindings)               (populate 1 maker (field-label ...) . bindings))))))))


(define-syntax to-identifier
  (syntax-rules ()
    ((to-identifier #f k) (syntax-apply k generated-identifier))
    ((to-identifier id k) (syntax-apply k id))))

(define-syntax augment-field 
  (syntax-rules ()
    ((augment-field (label) k)               (syntax-apply k (label generated-getter generated-setter)))
    ((augment-field (label getter) k)        (meta `(label (meta ,(to-identifier getter)) generated-setter) k))
    ((augment-field (label getter setter) k) (meta `(label (meta ,(to-identifier getter)) 
                                                           (meta ,(to-identifier setter))) k))))

(define-syntax extend-predicates
  (syntax-rules ()
    ((extend-predicates (super ...) predicate)
     (begin
       (meta
        `(define-method (meta ,(super ("predicate")))
                        (predicate)
                        (x)
                        any?))   
       ...))))

(define-syntax extend-copiers
  (syntax-rules ()
    ((extend-copiers (super ...) copy predicate)
     (begin
       (meta
        `(define-method (meta ,(super ("copier")))
                        (predicate)
                        (x)
                        copy))    
       ...))))

(define-syntax extend-accessors
  (syntax-rules ()
    ((extend-accessors (super ...) label predicate selector modifier)
     (meta
      `(begin 
         (syntax-if (meta ,(super ("getter") label))
                    (define-method (meta ,(super ("getter") label))
                                   (predicate)
                                   (x)
                                   selector)
                    (begin))
         ...
         (syntax-if (meta ,(super ("setter") label))
                    (define-method (meta ,(super ("setter") label))
                                   (predicate any?)
                                   (x v)
                                   modifier)
                    (begin))
         ...)))))

(define-syntax populate
  (syntax-rules ()
    ((populate 1 maker labels . bindings)
     (meta 
      `(populate 2 maker
                   (meta ,(order labels bindings ('<undefined>))))))
    ((populate 2 maker ((label exp) ...))
     (maker exp ...))))

(define-syntax order
  (syntax-rules ()
    ((order (label ...) ((label* . binding) ...) default k)
     (meta
      `(if-empty? (meta ,(remove-from (label* ...) 
                                      (label ...) 
                                      if-free=))
                  (order "emit" (label ...) ((label* . binding) ...) default k)
                  (syntax-error "Illegal labels in" ((label* . binding) ...)
                                "Legal labels are" (label ...)))))
    ((order "emit" (label ...) bindings default k)
     (meta 
      `((label . (meta ,(syntax-lookup label 
                                       bindings 
                                       if-free= 
                                       default)))
        ...)
      k))))


;============================================================================================
; Simple generic functions:

(define-syntax define-generic
  (syntax-rules ()
    ((define-generic (name arg ...))
     (define-generic (name arg ...)
       (lambda (arg ...) (error "Inapplicable method:" 'name
                                "Arguments:" (show arg) ... ))))
    ((define-generic (name arg ...) proc)
     (define name (make-generic (arg ...) proc)))))  
  
(define-syntax define-method
  (syntax-rules ()
    ((define-method (generic (arg pred?) ...) . body)
     (define-method generic (pred? ...) (arg ...) (lambda (arg ...) . body))) 
    ((define-method generic (pred? ...) (arg ...) procedure)
     (let ((next ((generic) 'get-proc))
           (proc procedure))
       (((generic) 'set-proc)
        (lambda (arg ...)
          (if (and (pred? arg) ...)
              (proc arg ...)
              (next arg ...))))))))

(define-syntax make-generic
  (syntax-rules ()
    ((make-generic (arg arg+ ...) default-proc)
     (let ((proc default-proc))
       (case-lambda
         ((arg arg+ ...)
          (proc arg arg+ ...))
         (()
          (lambda (msg)
            (case msg
              ((get-proc) proc)
              ((set-proc) (lambda (new)
                            (set! proc new)))))))))))

(define-generic (show x) 
  (lambda (x) x))

(define (any? x) #t)


;============================================================================================
; Syntax utilities:

(define-syntax syntax-error
  (syntax-rules ()))

(define-syntax syntax-apply
  (syntax-rules ()
    ((syntax-apply (f . args) exp ...) 
     (f exp ... . args))))

(define-syntax syntax-cons
  (syntax-rules ()
    ((syntax-cons x rest k) 
     (syntax-apply k (x . rest)))))

(define-syntax syntax-cons-after
  (syntax-rules ()
    ((syntax-cons-after rest x k)
     (syntax-apply k (x . rest)))))

(define-syntax if-empty?
  (syntax-rules ()
    ((if-empty? () sk fk)      sk)
    ((if-empty? (h . t) sk fk) fk)))

(define-syntax add-temporaries   
  (syntax-rules () 
    ((add-temporaries lst k)                (add-temporaries lst () k))
    ((add-temporaries () lst-temps k)       (syntax-apply k lst-temps))
    ((add-temporaries (h . t) (done ...) k) (add-temporaries t (done ... (h temp)) k))))

(define-syntax if-free=
  (syntax-rules ()
    ((if-free= x y kt kf)
      (let-syntax
          ((test (syntax-rules (x)
                   ((test x kt* kf*) kt*)
                   ((test z kt* kf*) kf*))))
        (test y kt kf)))))

(define-syntax top:if-free=
  (syntax-rules ()
    ((top:if-free= x y kt kf)
     (begin
       (define-syntax if-free=:test
         (syntax-rules (x)
           ((if-free=:test x kt* kf*) kt*)
           ((if-free=:test z kt* kf*) kf*)))
       (if-free=:test y kt kf)))))

(define-syntax meta
  (syntax-rules (meta quasiquote unquote)
    ((meta `(meta ,(function argument ...)) k)
     (meta `(argument ...) (syntax-apply-to function k)))
    ((meta `(a . b) k)
     (meta `a (descend-right b k)))
    ((meta `whatever k) (syntax-apply k whatever))
    ((meta `arg)
     (meta `arg (syntax-id)))))

(define-syntax syntax-apply-to
  (syntax-rules ()
    ((syntax-apply-to (argument ...) function k)
     (function argument ... k))))

(define-syntax descend-right
  (syntax-rules ()
    ((descend-right evaled b k)
     (meta `b (syntax-cons-after evaled k)))))

(define-syntax syntax-id
  (syntax-rules ()
    ((syntax-id arg) arg))) 

(define-syntax remove-duplicates
  (syntax-rules ()
    ((remove-duplicates lst compare? k)
     (remove-duplicates lst () compare? k))
    ((remove-duplicates () done compare? k)
     (syntax-apply k done))
    ((remove-duplicates (h . t) (d ...) compare? k)
     (if-member? h (d ...) compare? 
                 (remove-duplicates t (d ...) compare? k)
                 (remove-duplicates t (d ... h) compare? k)))))

(define-syntax syntax-filter
  (syntax-rules ()
    ((syntax-filter () (if-p? arg ...) k)
     (syntax-apply k ()))
    ((syntax-filter (h . t) (if-p? arg ...) k)
     (if-p? h arg ...
            (syntax-filter t (if-p? arg ...) (syntax-cons-after h k))
            (syntax-filter t (if-p? arg ...) k)))))

(define-syntax if-member?
  (syntax-rules ()
    ((if-member? x () compare? sk fk) 
     fk)
    ((if-member? x (h . t) compare? sk fk)
     (compare? x h
               sk
               (if-member? x t compare? sk fk)))))

(define-syntax union
  (syntax-rules ()
    ((union (x ...) ... compare? k)
     (remove-duplicates (x ... ...) compare? k))))

(define-syntax intersection
  (syntax-rules ()
    ((intersection list1 list2 compare? k)
     (syntax-filter list1 (if-member? list2 compare?) k))))

(define-syntax remove-from
  (syntax-rules ()
    ((remove-from list1 list2 compare? k)
     (syntax-filter list1 (if-not-member? list2 compare?) k))))

(define-syntax if-not-member?
  (syntax-rules ()
    ((if-not-member? x list compare? sk fk)
     (if-member? x list compare? fk sk))))

(define-syntax generate-identifier
  (syntax-rules ()
    ((generate-identifier k) (syntax-apply k generated-identifier))))

(define-syntax syntax-if
  (syntax-rules ()
    ((syntax-if #f sk fk)    fk)
    ((syntax-if other sk fk) sk)))

(define-syntax syntax-lookup
  (syntax-rules ()
    ((syntax-lookup label () compare fail k)
     (syntax-apply k fail))
    ((syntax-lookup label ((label* . value) . bindings) compare fail k)
     (compare label label*
              (syntax-apply k value)
              (syntax-lookup label bindings compare fail k)))))

