(library (ironscheme trace)
  (export
    make-traced-macro)
  (import 
    (ironscheme)
    (except (ironscheme core) make-traced-macro))
    
  (define make-traced-macro 
    (lambda (name x) 
      (cond 
        [(procedure? x)  
         (make-traced-procedure name x syntax->datum)] 
        [(variable-transformer? x) 
         (make-variable-transformer 
           (make-traced-procedure name  
             (variable-transformer-procedure x) 
             syntax->datum))] 
        [else x])))
        
)        
        
          


