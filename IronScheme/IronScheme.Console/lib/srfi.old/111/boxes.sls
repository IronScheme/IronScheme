(library (srfi :111 boxes)
  (export box box? unbox set-box!)
  (import (rnrs))
  
  (define-record-type
    (box-type box box?)
    (fields
      (mutable value unbox set-box!))))