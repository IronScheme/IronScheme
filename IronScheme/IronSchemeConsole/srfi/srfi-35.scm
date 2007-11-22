;The reference implementation makes use of SRFI 1 ("List Library"), SRFI 9 ("Defining Record Types"), and SRFI 23 ("Error reporting mechanism").

(define-record-type :condition-type
  (really-make-condition-type name supertype fields all-fields)
  condition-type?
  (name condition-type-name)
  (supertype condition-type-supertype)
  (fields condition-type-fields)
  (all-fields condition-type-all-fields))

(define (make-condition-type name supertype fields)
  (if (not (symbol? name))
      (error "make-condition-type: name is not a symbol"
             name))
  (if (not (condition-type? supertype))
      (error "make-condition-type: supertype is not a condition type"
             supertype))
  (if (not
       (null? (lset-intersection eq?
                                 (condition-type-all-fields supertype)
                                 fields)))
      (error "duplicate field name" ))
  (really-make-condition-type name
                              supertype
                              fields
                              (append (condition-type-all-fields supertype)
                                      fields)))

(define-syntax define-condition-type
  (syntax-rules ()
    ((define-condition-type ?name ?supertype ?predicate
       (?field1 ?accessor1) ...)
     (begin
       (define ?name
         (make-condition-type '?name
                              ?supertype
                              '(?field1 ...)))
       (define (?predicate thing)
         (and (condition? thing)
              (condition-has-type? thing ?name)))
       (define (?accessor1 condition)
         (condition-ref (extract-condition condition ?name)
                        '?field1))
       ...))))

(define (condition-subtype? subtype supertype)
  (let recur ((subtype subtype))
    (cond ((not subtype) #f)
          ((eq? subtype supertype) #t)
          (else
           (recur (condition-type-supertype subtype))))))

(define (condition-type-field-supertype condition-type field)
  (let loop ((condition-type condition-type))
    (cond ((not condition-type) #f)
          ((memq field (condition-type-fields condition-type))
           condition-type)
          (else
           (loop (condition-type-supertype condition-type))))))

; The type-field-alist is of the form
; ((<type> (<field-name> . <value>) ...) ...)
(define-record-type :condition
  (really-make-condition type-field-alist)
  condition?
  (type-field-alist condition-type-field-alist))

(define (make-condition type . field-plist)
  (let ((alist (let label ((plist field-plist))
                 (if (null? plist)
                            '()
                     (cons (cons (car plist)
                                 (cadr plist))
                           (label (cddr plist)))))))
    (if (not (lset= eq?
                    (condition-type-all-fields type)
                    (map car alist)))
        (error "condition fields don't match condition type"))
    (really-make-condition (list (cons type alist)))))

(define (condition-has-type? condition type)
  (any (lambda (has-type)
         (condition-subtype? has-type type))
       (condition-types condition)))

(define (condition-ref condition field)
  (type-field-alist-ref (condition-type-field-alist condition)
                        field))

(define (type-field-alist-ref type-field-alist field)
  (let loop ((type-field-alist type-field-alist))
    (cond ((null? type-field-alist)
           (error "type-field-alist-ref: field not found"
                  type-field-alist field))
          ((assq field (cdr (car type-field-alist)))
           => cdr)
          (else
           (loop (cdr type-field-alist))))))

(define (make-compound-condition condition-1 . conditions)
  (really-make-condition
   (apply append (map condition-type-field-alist
                      (cons condition-1 conditions)))))

(define (extract-condition condition type)
  (let ((entry (find (lambda (entry)
                              (condition-subtype? (car entry) type))
                            (condition-type-field-alist condition))))
    (if (not entry)
        (error "extract-condition: invalid condition type"
                      condition type))
    (really-make-condition
      (list (cons type
                  (map (lambda (field)
                         (assq field (cdr entry)))
                       (condition-type-all-fields type)))))))

(define-syntax condition
  (syntax-rules ()
    ((condition (?type1 (?field1 ?value1) ...) ...)
     (type-field-alist->condition
      (list
       (cons ?type1
             (list (cons '?field1 ?value1) ...))
       ...)))))

(define (type-field-alist->condition type-field-alist)
  (really-make-condition
   (map (lambda (entry)
          (cons (car entry)
                (map (lambda (field)
                       (or (assq field (cdr entry))
                           (cons field
                                 (type-field-alist-ref type-field-alist field))))
                     (condition-type-all-fields (car entry)))))
        type-field-alist)))

(define (condition-types condition)
  (map car (condition-type-field-alist condition)))

(define (check-condition-type-field-alist the-type-field-alist)
  (let loop ((type-field-alist the-type-field-alist))
    (if (not (null? type-field-alist))
        (let* ((entry (car type-field-alist))
               (type (car entry))
               (field-alist (cdr entry))
               (fields (map car field-alist))
               (all-fields (condition-type-all-fields type)))
          (for-each (lambda (missing-field)
                      (let ((supertype
                             (condition-type-field-supertype type missing-field)))
                        (if (not
                             (any (lambda (entry)
                                    (let ((type (car entry)))
                                      (condition-subtype? type supertype)))
                                  the-type-field-alist))
                            (error "missing field in condition construction"
                                   type
                                   missing-field))))
                    (lset-difference eq? all-fields fields))
          (loop (cdr type-field-alist))))))

(define &condition (really-make-condition-type '&condition
                                               #f
                                               '()
                                               '()))

(define-condition-type &message &condition
  message-condition?
  (message condition-message))

(define-condition-type &serious &condition
  serious-condition?)

(define-condition-type &error &serious
  error?)
  
#|
;;; EXAMPLES

(define-condition-type &c &condition
  c?
  (x c-x))

(define-condition-type &c1 &c
  c1?
  (a c1-a))

(define-condition-type &c2 &c
  c2?
  (b c2-b))
(define v1 (make-condition &c1 'x "V1" 'a "a1"))

(c? v1)        ;=> #t
(c1? v1)       ;=> #t
(c2? v1)       ;=> #f
(c-x v1)       ;=> "V1"
(c1-a v1)      ;=> "a1"

(define v2 (condition (&c2
                        (x "V2")
                        (b "b2"))))

(c? v2)        ;=> #t
(c1? v2)       ;=> #f
(c2? v2)       ;=> #t
(c-x v2)       ;=> "V2"
(c2-b v2)      ;=> "b2"

(define v3 (condition (&c1
                       (x "V3/1")
                       (a "a3"))
                      (&c2
                       (b "b3"))))

(c? v3)        ;=> #t
(c1? v3)       ;=> #t
(c2? v3)       ;=> #t
(c-x v3)       ;=> "V3/1"
(c1-a v3)      ;=> "a3"
(c2-b v3)      ;=> "b3"

(define v4 (make-compound-condition v1 v2))

(c? v4)        ;=> #t
(c1? v4)       ;=> #t
(c2? v4)       ;=> #t
(c-x v4)       ;=> "V1"
(c1-a v4)      ;=> "a1"
(c2-b v4)      ;=> "b2"

(define v5 (make-compound-condition v2 v3))

(c? v5)        ;=> #t
(c1? v5)       ;=> #t
(c2? v5)       ;=> #t
(c-x v5)       ;=> "V2"
(c1-a v5)      ;=> "a3"
(c2-b v5)      ;=> "b2"

|#  
