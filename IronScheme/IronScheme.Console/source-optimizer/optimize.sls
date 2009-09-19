(library (source-optimizer optimize)
  (export 
    optimize 
    cp0-effort-limit 
    cp0-size-limit
    transform-input
    transform-output)
  (import 
    (rnrs)
    (source-optimizer cp0)
    (source-optimizer transform))

  (define (optimize expr)
    (let ((in (transform-input))
          (out (transform-output)))
      (out (cp0-optimize (in expr)))))

)
