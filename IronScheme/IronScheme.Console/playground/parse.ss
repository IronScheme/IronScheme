;; this program just reads a file and collects statistics on word frequency

(import 
  (ironscheme)
  (ironscheme linq)
  (ironscheme regex))

(define ht (make-hashtable string-hash string=?))

(define p (open-input-file "test.log"))

(define re (make-regex "\\s+"))

(define (fxadd1 n) (fx+ 1 n))

(let loop ((line (get-line p)))
  (unless (eof-object? line)  
    (let ((tokens (regex-split line re)))
      (vector-for-each
        (lambda (t)
          (hashtable-update! ht t fxadd1 0))
        tokens))
    (loop (get-line p))))

(close-port p)

(for-each
  (lambda (item)
    (printf "~a ~a\n" (car item) (cdr item)))        
  (from x in (hashtable-map ht cons)
   orderby (cdr x) descending
   select (cons (cdr x) (car x))))          
          

