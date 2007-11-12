(library (my-library)
  (export print-hello)
  (import (rnrs))
  (define (print-hello)
    (display "Hello World\n")))

