(library (ironscheme reader)
  (export
    read-annotated
    annotation?
    annotation-expression
    annotation-source
    annotation-stripped)
  (import 
    (rnrs base)
    (rnrs control)
    (ironscheme clr))
  
  (define read-annotated
    (case-lambda
      [()  (clr-call ironscheme.runtime.psyntax.annotatedreader readannotated '())]
      [(p) (clr-call ironscheme.runtime.psyntax.annotatedreader readannotated '() p)]))
  
  (define (annotation? x) 
    (clr-call ironscheme.runtime.psyntax.annotatedreader isannotation '() x))
  
  (define (annotation-expression x)
    (clr-call ironscheme.runtime.psyntax.annotatedreader annotationexpression '() x))
  
  (define (annotation-source x)
    (clr-call ironscheme.runtime.psyntax.annotatedreader annotationsource '() x))

  (define (annotation-stripped x)
    (clr-call ironscheme.runtime.psyntax.annotatedreader annotationstripped '() x))         
)
    