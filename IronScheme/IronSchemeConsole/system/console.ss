(library (system console)
  (export
    ;; read-only property
    out
    in
    error
    
    ;; method
    write
    )
    
  (import 
    (except (rnrs base) error write)
		(rnrs syntax-case)
    (ironscheme clr))
    
  (define-syntax out
    (syntax-rules ()
      [(_)                (clr-static-prop-get system.console out)]))

  (define-syntax in
    (syntax-rules ()
      [(_)                (clr-static-prop-get system.console in)]))

  (define-syntax error
    (syntax-rules ()
      [(_)                (clr-static-prop-get system.console error)]))

  (define-syntax write
		(syntax-rules ()
			[(_ obj)            (clr-static-call system.console write obj)]
			[(_ obj args ...)   (clr-static-call system.console write (clr-cast system.string obj) args ... )]))
      
)
