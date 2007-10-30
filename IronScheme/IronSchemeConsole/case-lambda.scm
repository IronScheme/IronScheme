
;; dotted lists dont work :(

(define-syntax case-lambda
  (syntax-rules ()
    ((_ (?a1 ?e1 ...) ?clause1 ...)
     (lambda args
       (let ((l (length args)))
         (case-lambda "CLAUSE" args l
                      (?a1 ?e1 ...)
                      ?clause1 ...))))
    ((_ "CLAUSE" ?args ?l ((?a1 ...) ?e1 ...) ?clause1 ...)
     (if (= ?l (length '(?a1 ...)))
         (apply (lambda (?a1 ...) ?e1 ...) ?args)
         (case-lambda "CLAUSE" ?args ?l
                      ?clause1 ...)))
    ((_ "CLAUSE" ?args ?l ((?a1 ... . ?ar) ?e1 ...) ?clause1 ...)
     (case-lambda "IMPROPER" ?args ?l 1 (?a1 ... . ?ar) (?ar ?e1 ...)
                  ?clause1 ...))
    ((_ "CLAUSE" ?args ?l (?a1 ?e1 ...)?clause1 ...)
     (let ((?a1 ?args))
       ?e1 ...))
    ((_ "CLAUSE" ?args ?l)
     (error "Wrong number of arguments to CASE-LAMBDA."))
    ((_ "IMPROPER" ?args ?l ?k ?al ((?a1 . ?ar) ?e1 ...)
        ?clause1 ...)
     (case-lambda "IMPROPER" ?args ?l (+ ?k 1) ?al (?ar ?e1 ...)
                  ?clause1 ...))
    ((_ "IMPROPER" ?args ?l ?k ?al (?ar ?e1 ...)
        ?clause1 ...)
     (if (>= ?l ?k)
         (apply (lambda ?al ?e1 ...) ?args)
         (case-lambda "CLAUSE" ?args ?l
                      ?clause1 ...)))))

