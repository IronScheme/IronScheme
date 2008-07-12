(library (ironscheme trace)
  (export
    make-traced-macro)
  (import 
    (rnrs)
    (ironscheme core))
    
  (define make-traced-macro 
    (lambda (name x) 
      (cond 
        [(procedure? x)  
         (make-traced-procedure name x)] 
        [(variable-transformer? x) 
         (make-variable-transformer 
           (make-traced-procedure name  
             (variable-transformer-procedure x) 
             syntax->datum))] 
        [else x])))) 


