(import 
    (ironscheme) 
    (ironscheme typed))

(define: (a -> fixnum) 
  (letrec*: 
    (((a : fixnum) 10)
     ((b : fixnum) (fx+ a 5))
     ((c : fixnum) (fx+ b 3))
     -> fixnum)
    (fx+ a (fx+ b c))))

(disassemble a)
(displayln (a))    

(define: (b -> fixnum)
  (letrec*:
    ((a 10)
     (b (fx+ a 5))
     (c (fx+ b 3)))
    (fx+ a (fx+ b c))))

(disassemble b)
(displayln (b))
