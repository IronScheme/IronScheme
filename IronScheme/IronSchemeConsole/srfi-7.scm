

(define-syntax program
  (syntax-rules (requires files code feature-cond)
    ((program)
     (begin))
    ((program (requires feature-id ...)
              more ...)
     (begin (cond-expand ((and feature-id ...) 'okay))
            (program more ...)))
    ((program (files filename ...)
              more ...)
     (begin (load filename) ...
            (program more ...)))
    ((program (code stuff ...)
              more ...)
     (begin stuff ...
            (program more ...)))
    ((program (feature-cond (requirement stuff ...) ...)
              more ...)
     (begin (cond-expand (requirement (program stuff ...)) ...)
            (program more ...)))))

