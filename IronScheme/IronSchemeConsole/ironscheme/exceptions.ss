(library (ironscheme exceptions)
  (export
    with-exception-handler
    guard
    raise
    else
    =>
    raise-continuable)
    
  (import (rnrs exceptions))
)