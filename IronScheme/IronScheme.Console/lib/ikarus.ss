; ikarus compatibility file, no more import renames!
(library (ikarus)
  (export)
  (import (ironscheme library-utils))

  (import-and-reexport-all-from 
    (ironscheme)))
    