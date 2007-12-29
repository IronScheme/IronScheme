(import (ironscheme))

(define old-library-locator (library-locator))

(library-path '("." "./lib"))

(library-locator 
  (lambda (x)
    (or (clr-library-locator x)
        (old-library-locator x)) ))

