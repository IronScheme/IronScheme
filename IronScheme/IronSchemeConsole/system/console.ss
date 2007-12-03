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
      [(_)                (clr-call system.console:get_out '())]))

  (define-syntax in
    (syntax-rules ()
      [(_)                (clr-call system.console:get_in '())]))

  (define-syntax error
    (syntax-rules ()
      [(_)                (clr-call system.console:get_error '())]))

  (define-syntax write
		(syntax-rules ()
			[(_ obj)            (clr-call system.console:write '() obj)]
			[(_ obj args ...)   (clr-call system.console:write '() (clr-cast system.string obj) args ... )]))
      
)
