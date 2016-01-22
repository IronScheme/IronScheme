#| License
Copyright (c) 2007-2016 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme regex-cond)
  (export regex-cond regex-cond* else)
  (import 
    (ironscheme)
    (ironscheme syntax utils)
    (ironscheme regex))
    
;;; CHALLENGE: How to refactor this? - define-for-syntax would help here
    
  (define-syntax regex-cond*
    (lambda (x)
      (define (wrap p)
        (string-append "^" (syntax->datum p) "$"))
      (define (get-groups pattern)
        (map (lambda (match)
               (group-value (match-group match "group")))
             (regex-matches pattern "\\(\\?\\<(?<group>\\w+)\\>")))
      (syntax-case x (else)
        [(ctx str (pattern e e* ...) ... (else ee ee* ...))
          (let ((parse-clause (lambda (c)
                                (syntax-case c ()
                                  [(pattern e e* ...)
                                    (let ((grps (get-groups (wrap (syntax->datum #'pattern)))))
                                      (with-syntax (((binding ...) (map (lambda (p) 
                                                                          (datum->syntax #'ctx (string->symbol p))) 
                                                                        grps))
                                                    ((group ...) grps)
                                                    (pattern (wrap #'pattern)))
                                        #'((let ((m (regex-match str pattern)))
                                            (and (group-success? m) m)) =>
                                              (lambda (m)
                                                (let ((binding (group-value (match-group m group))) ...)
                                                  e e* ...)))))]))))
            (with-syntax (((clause ...) (map parse-clause #'((pattern e e* ...) ...))))
              #'(cond
                  clause ...
                  [else ee ee* ...])))]
        [(ctx str (pattern e e* ...) ... )
          #'(ctx str (pattern e e* ...) ... (else #f))])))
    
  (define-syntax regex-cond
    (lambda (x)
      (define (get-groups pattern)
        (map (lambda (match)
               (group-value (match-group match "group")))
             (regex-matches pattern "\\(\\?\\<(?<group>\\w+)\\>")))
      (syntax-case x (else)
        [(ctx str (pattern e e* ...) ... (else ee ee* ...))
          (let ((parse-clause (lambda (c)
                                (syntax-case c ()
                                  [(pattern e e* ...)
                                    (let ((grps (get-groups (syntax->datum #'pattern))))
                                      (with-syntax (((binding ...) (map (lambda (p) 
                                                                          (datum->syntax #'ctx (string->symbol p))) 
                                                                        grps))
                                                    ((group ...) grps))
                                        #'((let ((m (regex-match str pattern)))
                                            (and (group-success? m) m)) =>
                                              (lambda (m)
                                                (let ((binding (group-value (match-group m group))) ...)
                                                  e e* ...)))))]))))
            (with-syntax (((clause ...) (map parse-clause #'((pattern e e* ...) ...))))
              #'(cond
                  clause ...
                  [else ee ee* ...])))]
        [(ctx str (pattern e e* ...) ... )
          #'(ctx str (pattern e e* ...) ... (else #f))]))))