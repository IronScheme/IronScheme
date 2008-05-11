#!r6rs
(library (ironscheme record-case)
  (export
    record-case)
  (import 
    (rnrs))

  (define (get-index vec sym)
    (let ((len (vector-length vec)))
      (let f ((i 0))
        (cond 
          [(= i len) #f]
          [(eq? sym (vector-ref vec i)) i]
          [else (f (+ i 1))]))))

  (define (get-fields r rtd . x)  
    (reverse
      (let ((names (record-type-field-names rtd)))
        (let f ((x x)(o '()))
          (if (null? x)
            o
            (let g ((i (get-index names (car x)))(rtd rtd))
              (if i
                (f (cdr x) (cons ((record-accessor rtd i) r) o))
                (let ((prtd (record-type-parent rtd)))
                  (if prtd
                    (g (get-index (record-type-field-names prtd) (car x)) prtd)
                    (assertion-violation 'get-fields "field not found" (car x) r))))))))))

  (define-syntax record-case
    (lambda (x)
      (syntax-case x (else)
        [(_ r [r? (f ...) e e* ...] ... [else ee])
          (for-all identifier? #'(r? ...))
          #'(let ((r* r))
              (cond
                [((record-predicate (record-type-descriptor r?)) r*)
                 (apply 
                  (lambda (f ...) e e* ...) 
                  (get-fields r* (record-type-descriptor r?) 'f ...) )] ...
                [else ee]))]
        [(_ r [r? (f ...) e] ... )
          #'(record-case r [r? (f ...) e] ... [else #f])]
          )))    
)
    