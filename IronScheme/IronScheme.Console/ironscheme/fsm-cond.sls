(library (ironscheme fsm-cond)
  (export fsm-cond)
  (import
    (ironscheme)
    (ironscheme fsm-cond-helpers))

  (define-syntax fsm-cond
    (lambda (x)
      (syntax-case x ()
        [(_ (id ...) ((pred ...) expr) ...)
          (for-all identifier? #'(pred ... ... id ...))
          (let* ((tree (make-tree)))
            (for-each (lambda (preds expr)
                        (tree-add! tree expr preds))
                      #'((pred ...) ...)
                      #'(expr ...))
            (generate-tree tree #'(id ...)))]))))
            