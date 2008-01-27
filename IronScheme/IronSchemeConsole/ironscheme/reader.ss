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
    
  (clr-using ironscheme.runtime.psyntax)
  
  (define read-annotated
    (case-lambda
      [()  (clr-static-call annotatedreader readannotated)]
      [(p) (clr-static-call annotatedreader readannotated p)]))
  
  (define (annotation? x) 
    (clr-static-call annotatedreader isannotation x))
  
  (define (annotation-expression x)
    (clr-static-call annotatedreader annotationexpression x))
  
  (define (annotation-source x)
    (clr-static-call annotatedreader annotationsource x))

  (define (annotation-stripped x)
    (clr-static-call annotatedreader annotationstripped x))         
    
  (clr-clear-usings)    
)
    