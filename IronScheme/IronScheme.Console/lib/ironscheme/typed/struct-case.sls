#| License
Copyright (c) 2011 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme typed struct-case)
  (export
    else
    struct-case)
  (import 
    (ironscheme)
    (ironscheme typed core)
    (ironscheme typed struct)
    (ironscheme typed struct-descriptor))

  (define-syntax struct-case
    (lambda (x)
      (syntax-case x (else)
        [(_ r [(r? f ...) e e* ...] ... [else ee])
          (for-all identifier? #'(r? ... f ... ...))
          (lambda (lookup)
            (with-syntax ([(((f t) ...) ...) (map (lambda (r f)
                                                    (let* ((sd (lookup r))
                                                           (flds (struct-descriptor-field-types sd)))
                                                      (map (lambda (f)
                                                             (list f (datum->syntax f (cdr (assq (syntax->datum f) flds)))))
                                                           f)))
                                                  #'(r? ...)
                                                  #'((f ...) ...))])
              #'(let ((r* r))
                  (cond
                    [((struct-predicate r?) r*)
                     ((lambda: ((f : t) ...) e e* ...)
                        ((struct-accessor r? f) r*) ...)] ...
                    [else ee]))))]
        [(_ r [(r? f ...) e  e* ...] ... )
          #'(struct-case r [(r? f ...) e e* ...] ... [else #f])]))))    