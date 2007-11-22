;;; Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

(library (psyntax expander)
  (export identifier? syntax-dispatch environment environment?
          eval expand generate-temporaries free-identifier=?
          bound-identifier=? datum->syntax syntax-error
          syntax->datum make-variable-transformer
          eval-r6rs-top-level boot-library-expand eval-top-level
          null-environment)
  (import
    (except (rnrs) 
      environment environment? identifier?
      eval generate-temporaries free-identifier=?
      bound-identifier=? datum->syntax
      syntax->datum make-variable-transformer
      null-environment)
    (rnrs base)
    (rnrs lists)
    (rnrs control)
    (rnrs io simple)
    (psyntax library-manager)
    (psyntax builders)
    (psyntax compat)
    (psyntax config)
    (psyntax internal)
    (only (rnrs syntax-case) syntax-case syntax with-syntax)
    (prefix (rnrs syntax-case) sys.))
  
  (define (set-cons x ls)
    (cond
      ((memq x ls) ls)
      (else (cons x ls))))
 
  (define (set-union ls1 ls2)
    (cond
      ((null? ls1) ls2)
      ((memq (car ls1) ls2) (set-union (cdr ls1) ls2))
      (else (cons (car ls1) (set-union (cdr ls1) ls2)))))
  
  (define-syntax no-source
    (lambda (x) #f))

  ;;; the body of a library, when it's first processed, gets this
  ;;; set of marks. 
  (define top-mark* '(top))

  ;;; consequently, every syntax object that has a top in its marks 
  ;;; set was present in the program source.
  (define top-marked?
    (lambda (m*) (memq 'top m*)))
  
  ;;; This procedure generates a fresh lexical name for renaming.
  ;;; It's also use it to generate temporaries.
  (define gen-lexical
    (lambda (sym)
      (cond
        ((symbol? sym) (gensym sym))
        ((stx? sym) (gen-lexical (id->sym sym)))
        (else (error 'gen-lexical "BUG: invalid arg" sym)))))
  
  ;;; gen-global is used to generate global names (e.g. locations
  ;;; for library exports).  We use gen-lexical since it works just 
  ;;; fine.
  (define (gen-global x) (gen-lexical x))

  ;;; every identifier in the program would have a label associated
  ;;; with it in its substitution.  gen-label generates such labels.
  ;;; the labels have to have read/write eq? invariance to support 
  ;;; separate compilation.
  (define gen-label
    (lambda (_) (gensym)))

  ;;; A rib is a record constructed at every lexical contour in the
  ;;; program to hold information about the variables introduced in that
  ;;; contour.  Adding an identifier->label mapping to an extensible rib
  ;;; is achieved by consing the identifier's name to the list of
  ;;; symbols, consing the identifier's list of marks to the rib's
  ;;; mark**, and consing the label to the rib's labels.

  (define-record rib (sym* mark** label* sealed/freq))
 
  (define make-empty-rib
    (lambda ()
      (make-rib '() '() '() #f)))
  
  ;;; For example, when processing a lambda's internal define, a new rib
  ;;; is created and is added to the body of the lambda expression.
  ;;; When an internal definition is encountered, a new entry for the
  ;;; identifier is added (via side effect) to the rib.  A rib may be
  ;;; extensible, or sealed.  An extensible rib looks like:
  ;;;  #<rib list-of-symbols list-of-list-of-marks list-of-labels #f>
  
  (define (extend-rib! rib id label)
    (define (find sym mark* sym* mark**)
      (and (pair? sym*)
           (or (and (eq? sym (car sym*))
                    (same-marks? mark* (car mark**)))
               (find sym mark* (cdr sym*) (cdr mark**)))))
    (when (rib-sealed/freq rib)
      (error 'extend-rib! "rib is sealed" rib))
    (let ((sym (id->sym id)) (mark* (stx-mark* id)))
      (let ((sym* (rib-sym* rib)))
        (when (and (memq sym (rib-sym* rib))
                   (find sym mark* sym* (rib-mark** rib)))
          ;;; signal an error if the identifier was already
          ;;; in the rib.
          (stx-error id "cannot redefine"))
        (set-rib-sym*! rib (cons sym sym*))
        (set-rib-mark**! rib (cons mark* (rib-mark** rib)))
        (set-rib-label*! rib (cons label (rib-label* rib))))))

  ;;; A rib can be sealed once all bindings are inserted.  To seal
  ;;; a rib, we convert the lists sym*, mark**, and label* to vectors 
  ;;; and insert a frequency vector in the sealed/freq field.
  ;;; The frequency vector is an optimization that allows the rib to
  ;;; reorganize itself by bubbling frequently used mappings to the 
  ;;; top of the rib.  The vector is maintained in non-descending
  ;;; order and an identifier's entry in the rib is incremented at
  ;;; every access.  If an identifier's frequency exceeds the
  ;;; preceeding one, the identifier's position is promoted to the
  ;;; top of its class (or the bottom of the previous class).

  (define (seal-rib! rib)
    (let ((sym* (rib-sym* rib)))
      (unless (null? sym*)
        ;;; only seal if rib is not empty.
        (let ((sym* (list->vector sym*)))
          (set-rib-sym*! rib sym*)
          (set-rib-mark**! rib
            (list->vector (rib-mark** rib)))
          (set-rib-label*! rib
            (list->vector (rib-label* rib)))
          (set-rib-sealed/freq! rib
            (make-vector (vector-length sym*) 0))))))

  (define (unseal-rib! rib)
    (when (rib-sealed/freq rib)
      (set-rib-sealed/freq! rib #f)
      (set-rib-sym*! rib (vector->list (rib-sym* rib)))
      (set-rib-mark**! rib (vector->list (rib-mark** rib)))
      (set-rib-label*! rib (vector->list (rib-label* rib)))))

  (define (increment-rib-frequency! rib idx)
    (let ((freq* (rib-sealed/freq rib)))
      (let ((freq (vector-ref freq* idx)))
        (let ((i
               (let f ((i idx))
                 (cond
                   ((zero? i) 0)
                   (else
                    (let ((j (- i 1)))
                      (cond
                        ((= freq (vector-ref freq* j)) (f j))
                        (else i))))))))
          (vector-set! freq* i (+ freq 1))
          (unless (= i idx)
            (let ((sym* (rib-sym* rib))
                  (mark** (rib-mark** rib))
                  (label* (rib-label* rib)))
              (let ((sym (vector-ref sym* idx)))
                (vector-set! sym* idx (vector-ref sym* i))
                (vector-set! sym* i sym))
              (let ((mark* (vector-ref mark** idx)))
                (vector-set! mark** idx (vector-ref mark** i))
                (vector-set! mark** i mark*))
              (let ((label (vector-ref label* idx)))
                (vector-set! label* idx (vector-ref label* i))
                (vector-set! label* i label))))))))

  (define make-full-rib ;;; it may be a good idea to seal this rib
    (lambda (id* label*)
      (make-rib (map id->sym id*) (map stx-mark* id*) label* #f)))

  ;;; Now to syntax objects which are records defined like:
  (define-record stx (expr mark* subst*)
    (lambda (x p)
      (display "#<syntax " p)
      (write (stx->datum x) p)
      (display ">" p)))

  ;;; First, let's look at identifiers, since they're the real 
  ;;; reason why syntax objects are here to begin with.
  ;;; An identifier is an stx whose expr is a symbol.
  ;;; In addition to the symbol naming the identifier, the identifer
  ;;; has a list of marks and a list of substitutions.
  ;;; The idea is that to get the label of an identifier, we look up
  ;;; the identifier's substitutions for a mapping with the same
  ;;; name and same marks (see same-marks? below).

  ;;; Since all the identifier->label bindings are encapsulated
  ;;; within the identifier, converting a datum to a syntax object
  ;;; (non-hygienically) is done simply by creating an stx that has
  ;;; the same marks and substitutions as the identifier.
  (define datum->stx
    (lambda (id datum)
      (make-stx datum (stx-mark* id) (stx-subst* id))))

  ;;; A syntax object may be wrapped or unwrapped, so what does that
  ;;; mean exactly?  
  ;;;
  ;;; A wrapped syntax object is just a way of saying it's an stx 
  ;;; record.  All identifiers are stx records (with a symbol in
  ;;; their expr field).  Other objects such as pairs and vectors 
  ;;; may be wrapped or unwrapped.  A wrapped pair is an stx whos
  ;;; expr is a pair.  An unwrapped pair is a pair whos car and cdr
  ;;; fields are themselves syntax objects (wrapped or unwrapped).
  ;;;
  ;;; We always maintain the invariant that we don't double wrap 
  ;;; syntax objects.  The only way to get a doubly-wrapped syntax
  ;;; object is by doing datum->stx (above) where the datum is
  ;;; itself a wrapped syntax object (r6rs may not even consider 
  ;;; wrapped syntax objects as datum, but let's not worry now).

  ;;; Syntax objects have, in addition to the expr, a 
  ;;; substitution field (stx-subst*).  The subst* is a list
  ;;; where each element is either a rib or the symbol "shift".
  ;;; Normally, a new rib is added to an stx at evert lexical
  ;;; contour of the program in order to capture the bindings
  ;;; inctroduced in that contour.  

  ;;; The mark* field of an stx is, well, a list of marks.
  ;;; Each of these marks can be either a generated mark 
  ;;; or an antimark.
  ;;; (two marks must be eq?-comparable, so we use a string
  ;;; of one char (this assumes that strings are mutable)).
  
  ;;; gen-mark generates a new unique mark
  (define (gen-mark) ;;; faster
    (string #\m))
  
  ;(define gen-mark ;;; useful for debugging
  ;  (let ((i 0))
  ;    (lambda () 
  ;      (set! i (+ i 1))
  ;      (string-append "m." (number->string i)))))
  
  ;;; We use #f as the anti-mark.
  (define anti-mark #f)
  (define anti-mark? not)
  
  ;;; So, what's an anti-mark and why is it there.
  ;;; The theory goes like this: when a macro call is encountered, 
  ;;; the input stx to the macro transformer gets an extra anti-mark,
  ;;; and the output of the transformer gets a fresh mark.
  ;;; When a mark collides with an anti-mark, they cancel one
  ;;; another.  Therefore, any part of the input transformer that
  ;;; gets copied to the output would have a mark followed
  ;;; immediately by an anti-mark, resulting in the same syntax
  ;;; object (no extra marks).  Parts of the output that were not
  ;;; present in the input (e.g. inserted by the macro transformer) 
  ;;; would have no anti-mark and, therefore, the mark would stick 
  ;;; to them.
  ;;; 
  ;;; Every time a mark is pushed to an stx-mark* list, a
  ;;; corresponding 'shift is pushed to the stx-subst* list.
  ;;; Every time a mark is cancelled by an anti-mark, the
  ;;; corresponding shifts are also cancelled.  

  ;;; The procedure join-wraps, here, is used to compute the new
  ;;; mark* and subst* that would result when the m1* and s1* are 
  ;;; added to an stx's mark* and subst*.
  ;;; The only tricky part here is that e may have an anti-mark 
  ;;; that should cancel with the last mark in m1*.
  ;;; So, if m1* is (mx* ... mx)
  ;;;    and m2* is (#f my* ...) 
  ;;; then the resulting marks should be (mx* ... my* ...)
  ;;; since mx would cancel with the anti-mark.
  ;;; The substs would have to also cancel since 
  ;;;     s1* is (sx* ... sx)
  ;;; and s2* is (sy sy* ...) 
  ;;; then the resulting substs should be (sx* ... sy* ...)
  ;;; Notice that both sx and sy would be shift marks.
  (define join-wraps
    (lambda (m1* s1* e)
      (define cancel
        (lambda (ls1 ls2)
          (let f ((x (car ls1)) (ls1 (cdr ls1)))
            (if (null? ls1)
                (cdr ls2)
                (cons x (f (car ls1) (cdr ls1)))))))
      (let ((m2* (stx-mark* e)) (s2* (stx-subst* e)))
        (if (and (not (null? m1*))
                 (not (null? m2*))
                 (anti-mark? (car m2*)))
            ; cancel mark, anti-mark, and corresponding shifts
            (values (cancel m1* m2*) (cancel s1* s2*))
            (values (append m1* m2*) (append s1* s2*))))))

  ;;; The procedure mkstx is then the proper constructor for
  ;;; wrapped syntax objects.  It takes a syntax object, a list
  ;;; of marks, and a list of substs.  It joins the two wraps 
  ;;; making sure that marks and anti-marks and corresponding 
  ;;; shifts cancel properly.
  (define mkstx
    (lambda (e m* s*)
      (if (stx? e)
          (let-values (((m* s*) (join-wraps m* s* e)))
            (make-stx (stx-expr e) m* s*))
          (make-stx e m* s*))))
  
  ;;; to add a mark, we always add a corresponding shift.
  (define add-mark
    (lambda (m e)
      (mkstx e (list m) '(shift))))

  (define add-subst
    (lambda (subst e)
      (mkstx e '() (list subst))))

  ;;; now are some deconstructors and predicates for syntax objects.
  (define syntax-kind?
    (lambda (x p?)
      (if (stx? x)
          (syntax-kind? (stx-expr x) p?)
          (p? x))))
  (define syntax-vector->list
    (lambda (x)
      (cond
        ((stx? x)
         (let ((ls (syntax-vector->list (stx-expr x)))
               (m* (stx-mark* x)) (s* (stx-subst* x)))
           (map (lambda (x) (mkstx x m* s*)) ls)))
        ((vector? x) (vector->list x))
        (else (error 'syntax-vector->list "not a syntax vector" x)))))
  (define syntax-pair?
    (lambda (x) (syntax-kind? x pair?)))
  (define syntax-vector?
    (lambda (x) (syntax-kind? x vector?)))
  (define syntax-null?
    (lambda (x) (syntax-kind? x null?)))
  (define syntax-list? ;;; FIXME: should terminate on cyclic input.
    (lambda (x)
      (or (syntax-null? x)
          (and (syntax-pair? x) (syntax-list? (syntax-cdr x))))))
  (define syntax-car
    (lambda (x)
      (if (stx? x)
          (mkstx (syntax-car (stx-expr x)) (stx-mark* x) (stx-subst* x))
          (if (pair? x)
              (car x)
              (error 'syntax-car "not a pair" x)))))
  (define syntax->list
    (lambda (x)
      (if (syntax-pair? x)
          (cons (syntax-car x) (syntax->list (syntax-cdr x)))
          (if (syntax-null? x)
              '()
              (error 'syntax->list "invalid argument" x)))))
  (define syntax-cdr
    (lambda (x)
      (if (stx? x)
          (mkstx (syntax-cdr (stx-expr x)) (stx-mark* x) (stx-subst* x))
          (if (pair? x)
              (cdr x)
              (error 'syntax-cdr "not a pair" x)))))
  (define id?
    (lambda (x) (syntax-kind? x symbol?)))
  
  (define id->sym
    (lambda (x)
      (if (stx? x)
          (id->sym (stx-expr x))
          (if (symbol? x)
              x
              (error 'id->sym "not an id" x)))))

  ;;; Two lists of marks are considered the same if they have the 
  ;;; same length and the corresponding marks on each are eq?.
  (define same-marks?
    (lambda (x y)
      (or (and (null? x) (null? y)) ;(eq? x y)
          (and (pair? x) (pair? y)
               (eq? (car x) (car y))
               (same-marks? (cdr x) (cdr y))))))
  
  ;;; Two identifiers are bound-id=? if they have the same name and
  ;;; the same set of marks.
  (define bound-id=?
    (lambda (x y)
      (and (eq? (id->sym x) (id->sym y))
           (same-marks? (stx-mark* x) (stx-mark* y)))))

  ;;; Two identifiers are free-id=? if either both are bound to the
  ;;; same label or if both are unbound and they have the same name.
  (define free-id=?
    (lambda (i j)
      (let ((t0 (id->label i)) (t1 (id->label j)))
        (if (or t0 t1)
            (eq? t0 t1)
            (eq? (id->sym i) (id->sym j))))))

  ;;; valid-bound-ids? takes checks if a list is made of identifers
  ;;; none of which is bound-id=? to another.
  (define valid-bound-ids?
    (lambda (id*)
      (and (for-all id? id*)
           (distinct-bound-ids? id*))))

  (define distinct-bound-ids?
    (lambda (id*)
      (or (null? id*)
          (and (not (bound-id-member? (car id*) (cdr id*)))
               (distinct-bound-ids? (cdr id*))))))

  (define bound-id-member?
    (lambda (id id*)
      (and (pair? id*)
           (or (bound-id=? id (car id*))
               (bound-id-member? id (cdr id*))))))

  (define self-evaluating?
    (lambda (x) ;;; am I missing something here?
      (or (number? x) (string? x) (char? x) (boolean? x))))

  ;;; strip is used to remove the wrap of a syntax object.
  ;;; It takes an stx's expr and marks.  If the marks contain
  ;;; a top-mark, then the expr is returned.  
  (define strip
    (lambda (x m*)
      (if (top-marked? m*)
          x
          (let f ((x x))
            (cond
              ((stx? x) (strip (stx-expr x) (stx-mark* x)))
              ((pair? x)
               (let ((a (f (car x))) (d (f (cdr x))))
                 (if (and (eq? a (car x)) (eq? d (cdr x)))
                     x
                     (cons a d))))
              ((vector? x)
               (let ((old (vector->list x)))
                 (let ((new (map f old)))
                   (if (for-all eq? old new)
                       x
                       (list->vector new)))))
              (else x))))))

  (define stx->datum
    (lambda (x)
      (strip x '())))

  ;;; id->label takes an id (that's a sym x marks x substs) and
  ;;; searches the substs for a label associated with the same sym
  ;;; and marks.
  (define id->label
    (lambda (id)
      (let ((sym (id->sym id)))
        (let search ((subst* (stx-subst* id)) (mark* (stx-mark* id)))
          (cond
            ((null? subst*)
             ;;; try to hook up the symbol from the interaction
             ;;; environment if there is one.
             (interaction-sym->label sym))
            ((eq? (car subst*) 'shift) 
             ;;; a shift is inserted when a mark is added.
             ;;; so, we search the rest of the substitution
             ;;; without the mark.
             (search (cdr subst*) (cdr mark*)))
            (else
             (let ((rib (car subst*)))
               (cond
                 ((rib-sealed/freq rib)
                  (let ((sym* (rib-sym* rib)))
                    (let f ((i 0) (j (vector-length sym*)))
                      (cond
                        ((= i j) (search (cdr subst*) mark*))
                        ((and (eq? (vector-ref sym* i) sym)
                              (same-marks? mark*
                                (vector-ref (rib-mark** rib) i)))
                          (let ((label (vector-ref (rib-label* rib) i)))
                            (increment-rib-frequency! rib i)
                            label))
                        (else (f (+ i 1) j))))))
                 (else
                  (let f ((sym* (rib-sym* rib))
                          (mark** (rib-mark** rib))
                          (label* (rib-label* rib)))
                    (cond
                      ((null? sym*) (search (cdr subst*) mark*))
                      ((and (eq? (car sym*) sym)
                            (same-marks? (car mark**) mark*))
                       (car label*))
                      (else (f (cdr sym*) (cdr mark**) (cdr label*))))))))))))))
  
  ;;; label->binding looks up the label in the environment r as 
  ;;; well as in the global environment.  Since all labels are
  ;;; unique, it doesn't matter which environment we consult first.
  ;;; we lookup the global environment first because it's faster
  ;;; (uses a hash table) while the lexical environment is an alist.
  ;;; If we don't find the binding of a label, we return the binding
  ;;; (displaced-lexical . #f) to indicate such.
  (define label->binding
    (lambda (x r)
      (cond
        ((imported-label->binding x) =>
         (lambda (b) 
           (if (and (pair? b) (eq? (car b) '$core-rtd)) 
               (cons '$rtd (map bless (cdr b)))
               b)))
        ((assq x r) => cdr)
        (else '(displaced-lexical . #f)))))

  (define make-binding cons)
  (define binding-type car)
  (define binding-value cdr)
  
  ;;; the type of an expression is determined by two things:
  ;;; - the shape of the expression (identifier, pair, or datum)
  ;;; - the binding of the identifier (for id-stx) or the type of
  ;;;   car of the pair.
  (define syntax-type
    (lambda (e r)
      (cond
        ((id? e)
         (let ((id e))
           (let* ((label (id->label id))
                  (b (label->binding label r))
                  (type (binding-type b)))
             (unless label ;;; fail early.
               (stx-error e "unbound identifier"))
             (case type
               ((lexical core-prim macro macro! global local-macro
                 local-macro! global-macro global-macro!
                 displaced-lexical syntax import $module $core-rtd)
                (values type (binding-value b) id))
               (else (values 'other #f #f))))))
        ((syntax-pair? e)
         (let ((id (syntax-car e)))
           (if (id? id)
               (let* ((label (id->label id))
                      (b (label->binding label r))
                      (type (binding-type b)))
                 (unless label ;;; fail early.
                   (stx-error e "unbound identifier"))
                 (case type
                   ((define define-syntax core-macro begin macro
                      macro! local-macro local-macro! global-macro
                      global-macro! module set! let-syntax 
                      letrec-syntax import $core-rtd)
                    (values type (binding-value b) id))
                   (else
                    (values 'call #f #f))))
               (values 'call #f #f))))
        (else (let ((d (stx->datum e)))
                (if (self-evaluating? d)
                    (values 'constant d #f)
                    (values 'other #f #f)))))))

  (define-syntax stx-error
    (lambda (x)
      (syntax-case x ()
        ((_ stx)
         (syntax (error 'expander "invalid syntax" (stx->datum stx))))
        ((_ stx msg) (syntax (error 'expander msg (strip stx '())))))))
  
  ;;; when the rhs of a syntax definition is evaluated, it should be
  ;;; either a procedure, an identifier-syntax transformer or an
  ;;; ($rtd . #<rtd>) form (ikarus/chez).  sanitize-binding converts
  ;;; the output to one of:
  ;;;   (lacal-macro . procedure)
  ;;;   (local-macro! . procedure)
  ;;;   ($rtd . $rtd)
  ;;; and signals an error otherwise.
  (define sanitize-binding
    (lambda (x src)
      (cond
        ((procedure? x)
         (cons* 'local-macro x src))
        ((and (pair? x) (eq? (car x) 'macro!) (procedure? (cdr x)))
         (cons* 'local-macro! (cdr x) src))
        ((and (pair? x) (eq? (car x) '$rtd)) x)
        (else (error 'expand "invalid transformer" x)))))
  
  ;;; r6rs's make-variable-transformer:
  (define make-variable-transformer
    (lambda (x)
      (if (procedure? x)
          (cons 'macro! x)
          (error 'make-variable-transformer
                 "not a procedure" x))))

  ;;; make-eval-transformer takes an expanded expression, 
  ;;; evaluates it and returns a proper syntactic binding
  ;;; for the resulting object.
  (define make-eval-transformer
    (lambda (x)
      (sanitize-binding (eval-core (expanded->core x)) x)))

  ;;; The syntax-match macro is almost like syntax-case macro.
  ;;; Except that:
  ;;;   The syntax objects matched are OUR stx objects, not
  ;;;     the host systems syntax objects (whatever they may be
  ;;;     we don't care).
  ;;;   The literals are matched against those in the system 
  ;;;     library (psyntax system $all).   -- see scheme-stx
  ;;;   The variables in the patters are bound to ordinary variables
  ;;;     not to special pattern variables.
  (define-syntax syntax-match
    (lambda (ctx)
      (define dots?
        (lambda (x)
          (and (sys.identifier? x)
               (sys.free-identifier=? x (syntax (... ...))))))
      (define free-identifier-member?
        (lambda (x ls)
          (and (exists (lambda (y) (sys.free-identifier=? x y)) ls) #t)))
      (define (parse-clause lits cls)
        (define (parse-pat pat)
          (syntax-case pat ()
            (id (sys.identifier? (syntax id))
             (cond
               ((free-identifier-member? (syntax id) lits)
                (values '()
                  (syntax
                    (lambda (x)
                       (and (id? x)
                         (free-id=? x (scheme-stx 'id))
                         '())))))
               ((sys.free-identifier=? (syntax id) (syntax _))
                (values '() (syntax (lambda (x) '()))))
               (else
                (values (list (syntax id)) (syntax (lambda (x) (list x)))))))
            ((pat dots) (dots? (syntax dots))
             (let-values (((pvars decon) (parse-pat (syntax pat))))
               (with-syntax (((v* ...) pvars) (decon decon))
                 (values pvars
                   (syntax (letrec ((f (lambda (x)
                                   (cond
                                     ((syntax-pair? x)
                                      (let ((cars/f (decon (syntax-car x))))
                                        (and cars/f
                                          (let ((cdrs/f (f (syntax-cdr x))))
                                            (and cdrs/f
                                              (map cons cars/f cdrs/f))))))
                                     ((syntax-null? x)
                                      (list (begin 'v* '()) ...))
                                     (else #f)))))
                       f))))))
            ((pat dots . last) (dots? (syntax dots))
             (let-values (((p1 d1) (parse-pat (syntax pat)))
                          ((p2 d2) (parse-pat (syntax last))))
               (with-syntax (((v* ...) (append p1 p2))
                             ((v1* ...) p1)
                             ((v2* ...) p2)
                             (d1 d1) (d2 d2))
                 (values (append p1 p2)
                   (syntax (letrec ((f (lambda (x)
                                   (cond
                                     ((syntax-pair? x)
                                      (let ((cars/f (d1 (syntax-car x))))
                                        (and cars/f
                                          (let ((d/f (f (syntax-cdr x))))
                                            (and d/f
                                              (cons (map cons cars/f (car d/f))
                                                    (cdr d/f)))))))
                                     (else
                                      (let ((d (d2 x)))
                                        (and d
                                          (cons (list (begin 'v1* '()) ...)
                                                d))))))))
                       (lambda (x)
                         (let ((x (f x)))
                           (and x (append (car x) (cdr x)))))))))))
            ((pat1 . pat2)
             (let-values (((p1 d1) (parse-pat (syntax pat1)))
                          ((p2 d2) (parse-pat (syntax pat2))))
               (with-syntax ((d1 d1) (d2 d2))
                 (values (append p1 p2)
                    (syntax (lambda (x)
                        (and (syntax-pair? x)
                          (let ((q (d1 (syntax-car x))))
                            (and q
                              (let ((r (d2 (syntax-cdr x))))
                                (and r (append q r))))))))))))
            (#(pats ...) 
             (let-values (((pvars d) (parse-pat (syntax (pats ...)))))
                (with-syntax ((d d))
                  (values pvars
                    (syntax (lambda (x)
                        (and (syntax-vector? x)
                             (d (syntax-vector->list x)))))))))
            (datum
             (values '()
               (syntax (lambda (x)
                   (and (equal? (stx->datum x) 'datum) '())))))))
        (syntax-case cls ()
          ((pat body)
           (let-values (((pvars decon) (parse-pat (syntax pat))))
             (with-syntax (((v* ...) pvars))
               (values decon
                      (syntax (lambda (v* ...) #t)) 
                       (syntax (lambda (v* ...) body))))))
          ((pat guard body)
           (let-values (((pvars decon) (parse-pat (syntax pat))))
             (with-syntax (((v* ...) pvars))
               (values decon
                      (syntax (lambda (v* ...) guard)) 
                       (syntax (lambda (v* ...) body))))))))
      (syntax-case ctx ()
        ((_ expr (lits ...)) (for-all sys.identifier? (syntax (lits ...)))
         (syntax (stx-error expr "invalid syntax")))
        ((_ expr (lits ...) cls cls* ...) (for-all sys.identifier?
                                                   (syntax (lits ...)))
         (let-values (((decon guard body)
                       (parse-clause (syntax (lits ...)) (syntax cls))))
           (with-syntax ((decon decon) (guard guard) (body body))
             (syntax (let ((t expr))
                 (let ((ls/false (decon t)))
                   (if (and ls/false (apply guard ls/false))
                       (apply body ls/false)
                       (syntax-match t (lits ...) cls* ...)))))))))))

  (define parse-define
    (lambda (x)
      ;;; FIXME:  (define f) is not supported yet
      (syntax-match x ()
        ((_ (id . fmls) b b* ...) (id? id)
         (values id (cons 'defun (cons fmls (cons b b*)))))
        ((_ id val) (id? id)
         (values id (cons 'expr val))))))

  (define parse-define-syntax
    (lambda (x)
      ;;; FIXME: (define-syntax (f stx) ---) is not supported yet
      (syntax-match x ()
        ((_ id val) (id? id) (values id val)))))

  ;;; scheme-stx takes a symbol and if it's in the 
  ;;; (psyntax system $all) library, it creates a fresh identifier
  ;;; that maps only the symbol to its label in that library.
  ;;; Symbols not in that library become fresh.
  (define scheme-stx
    (lambda (sym)
      (let ((subst
             (library-subst
               (find-library-by-name '(psyntax system $all)))))
        (cond
          ((assq sym subst) =>
           (lambda (x)
             (let ((name (car x)) (label (cdr x)))
               (add-subst
                 (make-rib (list name) (list top-mark*) (list label) #f)
                 (mkstx sym top-mark* '())))))
          (else (mkstx sym top-mark* '()))))))

  ;;; macros
  (define add-lexical
    (lambda (lab lex r)
      (cons (cons* lab 'lexical lex) r)))
  ;;;
  (define add-lexicals
    (lambda (lab* lex* r)
      (cond
        ((null? lab*) r)
        (else
         (add-lexicals (cdr lab*) (cdr lex*)
           (add-lexical (car lab*) (car lex*) r))))))
  ;;;
  (define let-values-transformer ;;; go away
    (lambda (e r mr)
      (syntax-match e ()
        ((_ (((fml** ...) rhs*) ...) b b* ...)
         (let ((rhs* (chi-expr* rhs* r mr)))
           (let ((lex** (map (lambda (ls) (map gen-lexical ls)) fml**))
                 (lab** (map (lambda (ls) (map gen-label ls)) fml**)))
             (let ((fml* (apply append fml**))
                   (lab* (apply append lab**))
                   (lex* (apply append lex**)))
               (let f ((lex** lex**) (rhs* rhs*))
                  (cond
                    ((null? lex**)
                     (chi-internal
                       (add-subst
                         (make-full-rib fml* lab*)
                         (cons b b*))
                       (add-lexicals lab* lex* r)
                       mr))
                    (else
                     (build-application no-source
                       (build-primref no-source 'call-with-values)
                       (list
                         (build-lambda no-source '() (car rhs*))
                         (build-lambda no-source (car lex**)
                           (f (cdr lex**) (cdr rhs*)))))))))))))))

  (define letrec-helper
    (lambda (e r mr build)
      (syntax-match e ()
        ((_ ((lhs* rhs*) ...) b b* ...)
         (if (not (valid-bound-ids? lhs*))
             (stx-error e "invalid identifiers")
             (let ((lex* (map gen-lexical lhs*))
                   (lab* (map gen-label lhs*)))
               (let ((rib (make-full-rib lhs* lab*))
                     (r (add-lexicals lab* lex* r)))
                 (let ((body (chi-internal (add-subst rib (cons b b*)) r mr))
                       (rhs* (chi-expr* (map (lambda (x) (add-subst rib x)) rhs*) r mr)))
                   (build no-source lex* rhs* body)))))))))

  (define letrec-transformer
    (lambda (e r mr) (letrec-helper e r mr build-letrec)))
  
  (define letrec*-transformer
    (lambda (e r mr) (letrec-helper e r mr build-letrec*)))

  (define type-descriptor-transformer
    (lambda (e r mr)
      (syntax-match e ()
        ((_ id) (id? id)
         (let* ((lab (id->label id))
                (b (label->binding lab r))
                (type (binding-type b)))
           (unless lab (stx-error e "unbound identifier"))
           (unless (and (eq? type '$rtd) (not (list? (binding-value b))))
             (stx-error e "invalid type"))
           (build-data no-source (binding-value b)))))))


  (define record-type-descriptor-transformer
    (lambda (e r mr)
      (syntax-match e ()
        ((_ id) (id? id)
         (let* ((lab (id->label id))
                (b (label->binding lab r))
                (type (binding-type b)))
           (unless lab (stx-error e "unbound identifier"))
           (unless (and (eq? type '$rtd) (list? (binding-value b)))
             (stx-error e "invalid type"))
           (chi-expr (car (binding-value b)) r mr))))))

  (define record-constructor-descriptor-transformer
    (lambda (e r mr)
      (syntax-match e ()
        ((_ id) (id? id)
         (let* ((lab (id->label id))
                (b (label->binding lab r))
                (type (binding-type b)))
           (unless lab (stx-error e "unbound identifier"))
           (unless (and (eq? type '$rtd) (list? (binding-value b)))
             (stx-error e "invalid type"))
           (chi-expr (cadr (binding-value b)) r mr))))))

  (define when-transformer ;;; go away
    (lambda (e r mr)
      (syntax-match e ()
        ((_ test e e* ...)
         (build-conditional no-source
           (chi-expr test r mr)
           (build-sequence no-source
             (chi-expr* (cons e e*) r mr))
           (build-void))))))

  (define unless-transformer ;;; go away
    (lambda (e r mr)
      (syntax-match e ()
        ((_ test e e* ...)
         (build-conditional no-source
           (chi-expr test r mr)
           (build-void)
           (build-sequence no-source
             (chi-expr* (cons e e*) r mr)))))))
  
  (define if-transformer
    (lambda (e r mr)
      (syntax-match e ()
        ((_ e0 e1 e2)
         (build-conditional no-source
           (chi-expr e0 r mr)
           (chi-expr e1 r mr)
           (chi-expr e2 r mr)))
        ((_ e0 e1)
         (build-conditional no-source
           (chi-expr e0 r mr)
           (chi-expr e1 r mr)
           (build-void))))))
 
  (define case-transformer ;;; go away
    (lambda (e r mr)
      (define build-one
        (lambda (t cls rest)
          (syntax-match cls ()
            (((d* ...) e e* ...)
             (build-conditional no-source
               (build-application no-source
                 (build-primref no-source 'memv)
                 (list t (build-data no-source (stx->datum d*))))
               (build-sequence no-source
                 (chi-expr* (cons e e*) r mr))
               rest))
            (else (stx-error e)))))
      (define build-last
        (lambda (t cls)
          (syntax-match cls ()
            (((d* ...) e e* ...)
             (build-one t cls (build-void)))
            ((else-kwd x x* ...)
             (if (and (id? else-kwd)
                      (free-id=? else-kwd (scheme-stx 'else)))
                 (build-sequence no-source
                   (chi-expr* (cons x x*) r mr))
                 (stx-error e)))
            (else (stx-error e)))))
      (syntax-match e ()
        ((_ expr)
         (build-sequence no-source
           (list (chi-expr expr r mr) (build-void))))
        ((_ expr cls cls* ...)
         (let ((t (gen-lexical 't)))
           (build-let no-source
              (list t) (list (chi-expr expr r mr))
              (let f ((cls cls) (cls* cls*))
                (cond
                  ((null? cls*) (build-last t cls))
                  (else
                   (build-one t cls
                     (f (car cls*) (cdr cls*))))))))))))
  
  (define quote-transformer
    (lambda (e r mr)
      (syntax-match e ()
        ((_ datum) (build-data no-source (stx->datum datum))))))
  
  (define case-lambda-transformer
    (lambda (e r mr)
      (syntax-match e ()
        ((_ (fmls* b* b** ...) ...)
         (let-values (((fmls* body*)
                       (chi-lambda-clause* fmls*
                         (map cons b* b**) r mr)))
           (build-case-lambda no-source fmls* body*))))))
  
  (define lambda-transformer
    (lambda (e r mr)
      (syntax-match e ()
        ((_ fmls b b* ...)
         (let-values (((fmls body)
                       (chi-lambda-clause fmls
                          (cons b b*) r mr)))
           (build-lambda no-source fmls body))))))
  
  (define bless
    (lambda (x)
      (mkstx
        (let f ((x x))
          (cond
            ((stx? x) x)
            ((pair? x) (cons (f (car x)) (f (cdr x))))
            ((symbol? x) (scheme-stx x))
            ((vector? x)
             (list->vector (map f (vector->list x))))
            (else x)))
        '() '())))
  
  (define with-syntax-macro
    (lambda (e)
      (syntax-match e ()
        ((_ ((fml* expr*) ...) b b* ...)
         (bless
           `(syntax-case (list . ,expr*) ()
              (,fml* (begin ,b . ,b*))))))))
  
  (define let-macro
    (lambda (stx)
      (syntax-match stx ()
        ((_ ((lhs* rhs*) ...) b b* ...)
         (if (valid-bound-ids? lhs*)
             (bless `((lambda ,lhs* ,b . ,b*) . ,rhs*))
             (stx-error stx "invalid bindings")))
        ((_ f ((lhs* rhs*) ...) b b* ...) (id? f)
         (if (valid-bound-ids? lhs*)
             (bless `(letrec ((,f (lambda ,lhs* ,b . ,b*)))
                        (,f . ,rhs*)))
             (stx-error stx "invalid syntax"))))))
  
  (define trace-lambda-macro
    (lambda (stx)
      (syntax-match stx ()
        ((_ who (fmls ...) b b* ...)
         (if (valid-bound-ids? fmls)
             (bless `(make-traced-procedure ',who
                       (lambda ,fmls ,b . ,b*)))
             (stx-error stx "invalid formals")))
        ((_  who (fmls ... . last) b b* ...)
         (if (valid-bound-ids? (cons last fmls))
             (bless `(make-traced-procedure ',who
                       (lambda (,@fmls . ,last) ,b . ,b*)))
             (stx-error stx "invalid formals"))))))
  
  (define trace-define-macro
    (lambda (stx)
      (syntax-match stx ()
        ((_ (who fmls ...) b b* ...)
         (if (valid-bound-ids? fmls)
             (bless `(define ,who
                       (make-traced-procedure ',who
                         (lambda ,fmls ,b . ,b*))))
             (stx-error stx "invalid formals")))
        ((_ (who fmls ... . last) b b* ...)
         (if (valid-bound-ids? (cons last fmls))
             (bless `(define ,who
                       (make-traced-procedure ',who
                         (lambda (,@fmls . ,last) ,b . ,b*))))
             (stx-error stx "invalid formals")))
        ((_ who expr)
         (if (id? who)
             (bless `(define ,who
                       (let ((v ,expr))
                         (if (procedure? v)
                             (make-traced-procedure ',who v)
                             (error 'trace-define
                                "not a procedure" v)))))
             (stx-error stx "invalid formals"))))))
  
  (define guard-macro
    (lambda (x)
      (define (gen-clauses con outerk clause*) 
        (define (f x k) 
          (syntax-match x (=>) 
            ((e => p) 
             (let ((t (gensym)))
               `(let ((,t ,e)) 
                  (if ,t (,p ,t) ,k))))
            ((e) 
             (let ((t (gensym)))
               `(let ((,t ,e))
                  (if ,t ,t ,k))))
            ((e v v* ...) 
             `(if ,e (begin ,v ,@v*) ,k))
            (_ (stx-error x "invalid guard clause"))))
        (define (f* x*)
          (syntax-match x* (else)
            (() 
             (let ((g (gensym)))
               (values `(,g (lambda () (raise ,con))) g)))
            (((else e e* ...))
             (values `(begin ,e ,@e*) #f))
            ((cls . cls*) 
             (let-values (((e g) (f* cls*)))
               (values (f cls e) g)))
            (others (stx-error others "invalid guard clause"))))
        (let-values (((code raisek) (f* clause*)))
          (if raisek
              `((call/cc
                  (lambda (,raisek)
                    (,outerk 
                      (lambda () ,code)))))
              `(,outerk (lambda () ,code)))))
      (syntax-match x ()
        ((_ (con clause* ...) b b* ...)
         (id? con)
         (let ((outerk (gensym)))
           (bless
             `((call/cc
                 (lambda (,outerk)
                   (lambda ()
                     (with-exception-handler
                       (lambda (,con)
                         ,(gen-clauses con outerk clause*))
                       (lambda () #f ,b ,@b*))))))))))))

  (define define-enumeration-macro
    (lambda (stx) 
      (define (set? x)
        (or (null? x) 
            (and (not (memq (car x) (cdr x)))
                 (set? (cdr x)))))
      (syntax-match stx ()
        ((_ name (id* ...) maker) 
         (and (id? name) (id? maker) (for-all id? id*))
         (let ((name* (syntax->datum id*)) (mk (gensym))) 
           (unless (set? name*) 
             (stx-error stx "duplicate names in enumeration set"))
           (bless 
             `(begin
                (define ,mk 
                  (enum-set-constructor 
                    (make-enumeration ',name*)))
                (define-syntax ,name 
                  (lambda (x) 
                    (syntax-case x () 
                      ((_ n)
                       (identifier? (syntax n))
                       (if (memq (syntax->datum (syntax n)) ',name*) 
                           (syntax 'n)
                           (syntax-error x
                              "not a member of set"
                              ',name*))))))
                (define-syntax ,maker
                  (lambda (x)
                    (syntax-case x ()
                      ((_ n* ...)
                       (begin
                         (for-each
                           (lambda (n) 
                              (unless (identifier? n) 
                                (syntax-error x "non-identifier argument"))
                              (unless (memq (syntax->datum n) ',name*)
                                (syntax-error n "not a member of set")))
                           (syntax (n* ...)))
                         (syntax (,mk '(n* ...)))))))))))))))

  (define time-macro
    (lambda (stx)
      (syntax-match stx ()
        ((_ expr)
         (bless `(time-it ',expr (lambda () ,expr)))))))
  
  (define delay-macro
    (lambda (stx)
      (syntax-match stx ()
        ((_ expr)
         (bless `(make-promise (lambda () ,expr)))))))
  
  (define assert-macro
    (lambda (stx)
      (syntax-match stx ()
        ((_ expr)
         (bless `(unless ,expr
                   (error 'assert "assertion failed" ',expr)))))))
  
  (define endianness-macro
    (lambda (stx)
      (syntax-match stx ()
        ((_ e)
         (case (syntax->datum e)
           ((little) (bless `'little))
           ((big)    (bless `'big))
           (else (stx-error stx "endianness must be big or little")))))))
  
  (define identifier-syntax-macro
    (lambda (stx)
      (syntax-match stx (set!)
        ((_ expr)
         (bless `(lambda (x)
                   (syntax-case x ()
                     (id (identifier? (syntax id)) (syntax ,expr))
                     ((id e* ...) (identifier? (syntax id))
                      (cons (syntax ,expr) (syntax (e* ...))))))))
        ((_ (id1 expr1) ((set! id2 expr2) expr3))
         (and (id? id1) (id? id2) (id? expr2))
         (bless `(cons 'macro!
                   (lambda (x)
                     (syntax-case x (set!)
                       (id (identifier? (syntax id)) (syntax ,expr1))
                       ((set! id ,expr2) (syntax ,expr3))
                       ((id e* ...) (identifier? (syntax id)) (syntax (,expr1 e* ...)))))))))))
  
  (define do-macro
    (lambda (stx)
      (define bind
        (lambda (x)
          (syntax-match x ()
            ((x init)      `(,x ,init ,x))
            ((x init step) `(,x ,init ,step))
            (_  (stx-error stx "invalid binding")))))
      (syntax-match stx ()
        ((_ (binding* ...)
            (test expr* ...)
            command* ...)
         (syntax-match (map bind binding*) ()
           (((x* init* step*) ...)
            (if (valid-bound-ids? x*)
                (bless
                  `(letrec ((loop
                             (lambda ,x*
                               (if ,test
                                 (begin (if #f #f) ,@expr*)
                                 (begin
                                   ,@command*
                                   (loop ,@step*))))))
                     (loop ,@init*)))
                (stx-error stx "invalid bindings"))))))))
  
  (define let*-macro
    (lambda (stx)
      (syntax-match stx ()
        ((_ ((lhs* rhs*) ...) b b* ...) (for-all id? lhs*)
         (bless
           (let f ((x* (map list lhs* rhs*)))
             (cond
               ((null? x*) `(let () ,b . ,b*))
               (else `(let (,(car x*)) ,(f (cdr x*)))))))))))
  
  (define or-macro
    (lambda (stx)
      (syntax-match stx ()
        ((_) #f)
        ((_ e e* ...)
         (bless
           (let f ((e e) (e* e*))
             (cond
               ((null? e*) `(begin #f ,e))
               (else
                `(let ((t ,e))
                   (if t t ,(f (car e*) (cdr e*))))))))))))
  
  (define and-macro
    (lambda (stx)
      (syntax-match stx ()
        ((_) #t)
        ((_ e e* ...)
         (bless
           (let f ((e e) (e* e*))
             (cond
               ((null? e*) `(begin #f ,e))
               (else `(if ,e ,(f (car e*) (cdr e*)) #f)))))))))
  
  (define cond-macro
    (lambda (stx)
      (syntax-match stx ()
        ((_ cls cls* ...)
         (bless
           (let f ((cls cls) (cls* cls*))
             (cond
               ((null? cls*)
                (syntax-match cls (else =>)
                  ((else e e* ...) `(begin ,e . ,e*))
                  ((e => p) `(let ((t ,e)) (if t (,p t))))
                  ((e) `(or ,e (if #f #f)))
                  ((e e* ...) `(if ,e (begin . ,e*)))
                  (_ (stx-error stx "invalid last clause"))))
               (else
                (syntax-match cls (else =>)
                  ((else e e* ...) (stx-error stx "incorrect position of keyword else"))
                  ((e => p) `(let ((t ,e)) (if t (,p t) ,(f (car cls*) (cdr cls*)))))
                  ((e) `(or ,e ,(f (car cls*) (cdr cls*))))
                  ((e e* ...) `(if ,e (begin . ,e*) ,(f (car cls*) (cdr cls*))))
                  (_ (stx-error stx "invalid last clause")))))))))))
  
  (define include-macro
    (lambda (e)
      (syntax-match e ()
        ((id filename)
         (let ((filename (stx->datum filename)))
           (unless (string? filename) (stx-error e))
           (with-input-from-file filename
             (lambda ()
               (let f ((ls '()))
                 (let ((x (read)))
                   (cond
                     ((eof-object? x)
                      (cons (bless 'begin)
                        (datum->stx id (reverse ls))))
                     (else (f (cons x ls)))))))))))))
  
  (define syntax-rules-macro
    (lambda (e)
      (syntax-match e ()
        ((_ (lits ...)
            (pat* tmp*) ...)
         (begin
           (unless (for-all
                     (lambda (x)
                       (and (id? x)
                            (not (free-id=? x (scheme-stx '...)))
                            (not (free-id=? x (scheme-stx '_)))))
                     lits)
             (stx-error e "invalid literals"))
           (bless `(lambda (x)
                     (syntax-case x ,lits
                       ,@(map (lambda (pat tmp)
                                `(,pat (syntax ,tmp)))
                              pat* tmp*)))))))))
  
  (define quasiquote-macro
    (let ()
      (define-syntax app
        (syntax-rules (quote)
          ((_ 'x arg* ...)
           (list (scheme-stx 'x) arg* ...))))
      (define-syntax app*
        (syntax-rules (quote)
          ((_ 'x arg* ... last)
           (cons* (scheme-stx 'x) arg* ... last))))
      (define quasicons*
        (lambda (x y)
          (let f ((x x))
            (if (null? x) y (quasicons (car x) (f (cdr x)))))))
      (define quasicons
        (lambda (x y)
          (syntax-match y (quote list)
            ((quote dy)
             (syntax-match x (quote)
               ((quote dx) (app 'quote (cons dx dy)))
               (_
                (syntax-match dy ()
                  (() (app 'list x))
                  (_  (app 'cons x y))))))
            ((list stuff ...)
             (app* 'list x stuff))
            (_ (app 'cons x y)))))
      (define quasiappend
        (lambda (x y)
          (let ((ls (let f ((x x))
                      (if (null? x)
                          (syntax-match y (quote)
                            ((quote ()) '())
                            (_ (list y)))
                          (syntax-match (car x) (quote)
                            ((quote ()) (f (cdr x)))
                            (_ (cons (car x) (f (cdr x)))))))))
            (cond
              ((null? ls) (app 'quote '()))
              ((null? (cdr ls)) (car ls))
              (else (app* 'append ls))))))
      (define quasivector
        (lambda (x)
          (let ((pat-x x))
            (syntax-match pat-x (quote)
              ((quote (x* ...)) (app 'quote (list->vector x*)))
              (_ (let f ((x x) (k (lambda (ls) (app* 'vector ls))))
                   (syntax-match x (quote list cons)
                     ((quote (x* ...))
                      (k (map (lambda (x) (app 'quote x)) x*)))
                     ((list x* ...)
                      (k x*))
                     ((cons x y)
                      (f y (lambda (ls) (k (cons x ls)))))
                     (_ (app 'list->vector pat-x)))))))))
      (define vquasi
        (lambda (p lev)
          (syntax-match p ()
            ((p . q)
             (syntax-match p (unquote unquote-splicing)
               ((unquote p ...)
                (if (= lev 0)
                    (quasicons* p (vquasi q lev))
                    (quasicons
                      (quasicons (app 'quote 'unquote)
                                 (quasi p (- lev 1)))
                      (vquasi q lev))))
               ((unquote-splicing p ...)
                (if (= lev 0)
                    (quasiappend p (vquasi q lev))
                    (quasicons
                      (quasicons
                        (app 'quote 'unquote-splicing)
                        (quasi p (- lev 1)))
                      (vquasi q lev))))
               (p (quasicons (quasi p lev) (vquasi q lev)))))
            (() (app 'quote '())))))
      (define quasi
        (lambda (p lev)
          (syntax-match p (unquote unquote-splicing quasiquote)
            ((unquote p)
             (if (= lev 0)
                 p
                 (quasicons (app 'quote 'unquote) (quasi (list p) (- lev 1)))))
            (((unquote p ...) . q)
             (if (= lev 0)
                 (quasicons* p (quasi q lev))
                 (quasicons
                   (quasicons (app 'quote 'unquote) (quasi p (- lev 1)))
                   (quasi q lev))))
            (((unquote-splicing p ...) . q)
             (if (= lev 0)
                 (quasiappend p (quasi q lev))
                 (quasicons
                   (quasicons
                     (app 'quote 'unquote-splicing)
                     (quasi p (- lev 1)))
                   (quasi q lev))))
            ((quasiquote p)
             (quasicons (app 'quote 'quasiquote) (quasi (list p) (+ lev 1))))
            ((p . q) (quasicons (quasi p lev) (quasi q lev)))
            (#(x ...) (not (stx? x)) (quasivector (vquasi x lev)))
            (p (app 'quote p)))))
      (lambda (x)
        (syntax-match x ()
          ((_ e) (quasi e 0))))))
  
  (define quasisyntax-macro
    (let () ;;; FIXME: not really correct
      (define quasi
        (lambda (p lev)
          (syntax-match p (unsyntax unsyntax-splicing quasisyntax)
            ((unsyntax p)
             (if (= lev 0)
                 (let ((g (gensym)))
                   (values (list g) (list p) g))
                 (let-values (((lhs* rhs* p) (quasi p (- lev 1))))
                   (values lhs* rhs* (list 'unsyntax p)))))
            (unsyntax (= lev 0)
             (stx-error p "incorrect use of unsyntax"))
            (((unsyntax-splicing p) . q)
             (let-values (((lhs* rhs* q) (quasi q lev)))
               (if (= lev 0)
                   (let ((g (gensym)))
                     (values (cons `(,g ...) lhs*) (cons p rhs*)
                        `(,g ... . ,q)))
                   (let-values (((lhs2* rhs2* p) (quasi p (- lev 1))))
                     (values (append lhs2* lhs*)
                             (append rhs2* rhs*)
                             `((unsyntax-splicing ,p) . ,q))))))
            (unsyntax-splicing (= lev 0)
             (stx-error p "incorrect use of unsyntax-splicing"))
            ((quasisyntax p)
             (let-values (((lhs* rhs* p) (quasi p (+ lev 1))))
                (values lhs* rhs* `(quasisyntax ,p))))
            ((p . q)
             (let-values (((lhs* rhs* p) (quasi p lev))
                          ((lhs2* rhs2* q) (quasi q lev)))
                (values (append lhs2* lhs*)
                        (append rhs2* rhs*)
                        (cons p q))))
            (#(x ...) (not (stx? p))
             (let-values (((lhs* rhs* x*)
                           (let f ((x x))
                             (cond
                               ((null? x) (values '() '() '()))
                               (else
                                (let-values (((lhs* rhs* a) (quasi (car x) lev)))
                                  (let-values (((lhs2* rhs2* d) (f (cdr x))))
                                     (values (append lhs* lhs2*)
                                             (append rhs* rhs2*)
                                             (cons a d)))))))))
                (values lhs* rhs* (list->vector x*))))
            (_ (values '() '() p)))))
      (lambda (x)
        (syntax-match x ()
          ((_ e)
           (let-values (((lhs* rhs* v) (quasi e 0)))
              (bless
                `(syntax-case (list ,@rhs*) ()
                   (,lhs* (syntax ,v))))))))))


  (define define-struct-macro
    (if-wants-define-struct
      (lambda (e)
        (define enumerate
          (lambda (ls)
            (let f ((i 0) (ls ls))
              (cond
                ((null? ls) '())
                (else (cons i (f (+ i 1) (cdr ls))))))))
        (define mkid
          (lambda (id str)
            (datum->stx id (string->symbol str))))
        (syntax-match e ()
          ((_ name (field* ...))
           (let* ((namestr (symbol->string (id->sym name)))
                  (fields (map id->sym field*))
                  (fieldstr* (map symbol->string fields))
                  (rtd (datum->stx name (make-struct-type namestr fields)))
                  (constr (mkid name (string-append "make-" namestr)))
                  (pred (mkid name (string-append namestr "?")))
                  (i* (enumerate field*))
                  (getters
                   (map (lambda (x)
                          (mkid name (string-append namestr "-" x)))
                        fieldstr*))
                  (setters
                   (map (lambda (x)
                          (mkid name (string-append "set-" namestr "-" x "!")))
                        fieldstr*)))
             (bless
               `(begin
                  (define-syntax ,name (cons '$rtd ',rtd))
                  (define ,constr
                    (lambda ,field*
                      ($struct ',rtd ,@field*)))
                  (define ,pred
                    (lambda (x) ($struct/rtd? x ',rtd)))
                  ,@(map (lambda (getter i)
                            `(define ,getter
                               (lambda (x)
                                 (if ($struct/rtd? x ',rtd)
                                     ($struct-ref x ,i)
                                     (error ',getter
                                            "not a struct of required type"
                                            x ',rtd)))))
                         getters i*)
                  ,@(map (lambda (setter i)
                            `(define ,setter
                               (lambda (x v)
                                 (if ($struct/rtd? x ',rtd)
                                     ($struct-set! x ,i v)
                                     (error ',setter
                                            "not a struct of required type"
                                            x ',rtd)))))
                         setters i*)))))))
      (lambda (stx)
        (stx-error stx "define-struct not supported"))))


  (define define-record-type-macro
    (lambda (x)
      (define (id ctxt . str*)
        (datum->syntax ctxt 
          (string->symbol 
            (apply string-append 
              (map (lambda (x) 
                     (cond
                       ((symbol? x) (symbol->string x))
                       ((string? x) x)
                       (else (error 'define-record-type "BUG"))))
                   str*)))))
      (define (get-record-name spec)
        (syntax-match spec ()
          ((foo make-foo foo?) foo)
          (foo foo)))
      (define (get-record-constructor-name spec ctxt)
        (syntax-match spec ()
          ((foo make-foo foo?) make-foo)
          (foo (id ctxt "make-" (stx->datum foo)))))
      (define (get-record-predicate-name spec ctxt)
        (syntax-match spec ()
          ((foo make-foo foo?) foo?)
          (foo (id ctxt (stx->datum foo) "?"))))
      (define (get-clause id ls)
        (syntax-match ls ()
          (() #f)
          (((x . rest) . ls)
           (if (free-id=? (bless id) x)
               `(,x . ,rest)
               (get-clause id ls)))))
      (define (foo-rtd-code ctxt name clause*) 
        (define (convert-field-spec* ls)
          (list->vector
            (map (lambda (x) 
                   (syntax-match x (mutable immutable)
                     ((mutable name . rest) `(mutable ,name))
                     ((immutable name . rest) `(immutable ,name))
                     (name `(immutable ,name))))
               ls)))
        (let ((parent-rtd-code 
               (syntax-match (get-clause 'parent clause*) ()
                 ((_ name) `(record-type-descriptor ,name))
                 (_ '#f)))
              (uid-code
               (syntax-match (get-clause 'nongenerative clause*) ()
                 ((_)     `',(gensym))
                 ((_ uid) `',uid)
                 (_       #f)))
              (sealed?
               (syntax-match (get-clause 'sealed? clause*) ()
                 ((_ #t) #t)
                 (_      #f)))
              (opaque?
               (syntax-match (get-clause 'opaque? clause*) ()
                 ((_ #t) #t)
                 (_      #f)))
              (fields 
               (syntax-match (get-clause 'fields clause*) ()
                 ((_ field-spec* ...)
                  `(quote ,(convert-field-spec* field-spec*)))
                 (_ ''#()))))
          (bless
            `(make-record-type-descriptor ',name
               ,parent-rtd-code 
               ,uid-code ,sealed? ,opaque? ,fields))))
      (define (foo-rcd-code clause* foo-rtd protocol) 
        (let ((parent-rcd-code 
               (syntax-match (get-clause 'parent clause*) ()
                 ((_ name) `(record-constructor-descriptor ,name))
                 (_ #f))))
          `(make-record-constructor-descriptor ,foo-rtd
               ,parent-rcd-code ,protocol)))
      (define (get-protocol-code clause*)
        (syntax-match (get-clause 'protocol clause*) ()
          ((_ expr) expr)
          (_        #f)))
      (define (get-fields clause*)
        (syntax-match clause* (fields)
          (() '())
          (((fields f* ...) . _) f*)
          ((_ . rest) (get-fields rest))))
      (define (get-mutator-indices fields)
        (let f ((fields fields) (i 0))
          (syntax-match fields (mutable)
            (() '())
            (((mutable . _) . rest)
             (cons i (f rest (+ i 1))))
            ((_ . rest)
             (f rest (+ i 1))))))
      (define (get-mutators foo fields ctxt)
        (define (gen-name x) 
          (datum->syntax ctxt
            (string->symbol 
              (string-append "set-" 
                (symbol->string (syntax->datum foo))
                "-"
                (symbol->string (syntax->datum x))
                "!"))))
        (let f ((fields fields))
          (syntax-match fields (mutable)
            (() '())
            (((mutable name accessor mutator) . rest) 
             (cons mutator (f rest)))
            (((mutable name) . rest)
             (cons (gen-name name) (f rest)))
            ((_ . rest) (f rest)))))
      (define (get-accessors foo fields ctxt)
        (define (gen-name x) 
          (datum->syntax ctxt
            (string->symbol 
              (string-append
                (symbol->string (syntax->datum foo))
                "-"
                (symbol->string (syntax->datum x))))))
        (map 
          (lambda (field)
            (syntax-match field (mutable immutable)
              ((mutable name accessor mutator) (id? accessor) accessor)
              ((immutable name accessor)       (id? accessor) accessor)
              ((mutable name)                  (id? name) (gen-name name))
              ((immutable name)                (id? name) (gen-name name))
              (name                            (id? name) (gen-name name))
              (others (stx-error field "invalid field spec"))))
          fields))
      (define (enumerate ls)
        (let f ((ls ls) (i 0))
          (cond
            ((null? ls) '())
            (else (cons i (f (cdr ls) (+ i 1)))))))
      (define (do-define-record ctxt namespec clause*)
        (let* ((foo (get-record-name namespec))
               (foo-rtd (gensym))
               (foo-rcd (gensym))
               (protocol (gensym))
               (make-foo (get-record-constructor-name namespec ctxt))
               (fields (get-fields clause*))
               (idx* (enumerate fields))
               (foo-x* (get-accessors foo fields ctxt))
               (set-foo-x!* (get-mutators foo fields ctxt))
               (set-foo-idx* (get-mutator-indices fields))
               (foo? (get-record-predicate-name namespec ctxt))
               (foo-rtd-code (foo-rtd-code ctxt foo clause*))
               (foo-rcd-code (foo-rcd-code clause* foo-rtd protocol))
               (protocol-code (get-protocol-code clause*)))
          (bless
            `(begin
               (define ,foo-rtd ,foo-rtd-code)
               (define ,protocol ,protocol-code)
               (define ,foo-rcd ,foo-rcd-code)
               (define-syntax ,foo 
                 (list '$rtd (syntax ,foo-rtd) (syntax ,foo-rcd)))
               (define ,foo? (record-predicate ,foo-rtd))
               (define ,make-foo (record-constructor ,foo-rcd))
               ,@(map 
                   (lambda (foo-x idx)
                     `(define ,foo-x (record-accessor ,foo-rtd ,idx)))
                   foo-x* idx*)
               ,@(map 
                   (lambda (set-foo-x! idx)
                     `(define ,set-foo-x! (record-mutator ,foo-rtd ,idx)))
                   set-foo-x!* set-foo-idx*)))))
      (syntax-match x ()
        ((ctxt namespec clause* ...)
         (do-define-record ctxt namespec clause*)))))
  
  (define define-condition-type-macro
    (lambda (x)
      (define (mkname name suffix)
        (datum->syntax name 
           (string->symbol 
             (string-append 
               (symbol->string (syntax->datum name))
               suffix))))
      (syntax-match x ()
        ((ctxt name super constructor predicate (field* accessor*) ...)
         (and (id? name) 
              (id? super)
              (id? constructor)
              (id? predicate)
              (for-all id? field*)
              (for-all id? accessor*))
         (let ((aux-accessor* (map (lambda (x) (gensym)) accessor*)))
           (bless
              `(begin
                 (define-record-type (,name ,constructor ,(gensym))
                    (parent ,super)
                    (fields ,@(map (lambda (field aux) 
                                     `(immutable ,field ,aux))
                                   field* aux-accessor*))
                    (nongenerative)
                    (sealed #f) (opaque #f))
                 (define ,predicate (condition-predicate
                                      (record-type-descriptor ,name)))
                 ,@(map 
                     (lambda (accessor aux)
                        `(define ,accessor 
                           (condition-accessor
                             (record-type-descriptor ,name) ,aux)))
                     accessor* aux-accessor*))))))))
  
  (define incorrect-usage-macro
    (lambda (e) (stx-error e "incorrect usage of auxilary keyword")))
  
  (define parameterize-transformer ;;; go away
    (lambda (e r mr)
      (syntax-match e ()
        ((_ () b b* ...)
         (chi-internal (cons b b*) r mr))
        ((_ ((olhs* orhs*) ...) b b* ...)
         (let ((lhs* (map (lambda (x) (gen-lexical 'lhs)) olhs*))
               (rhs* (map (lambda (x) (gen-lexical 'rhs)) olhs*))
               (t*   (map (lambda (x) (gen-lexical 't)) olhs*))
               (swap (gen-lexical 'swap)))
           (build-let no-source
             (append lhs* rhs*)
             (append (chi-expr* olhs* r mr) (chi-expr* orhs* r mr))
             (build-let no-source
               (list swap)
               (list (build-lambda no-source '()
                       (build-sequence no-source
                         (map (lambda (t lhs rhs)
                                (build-let no-source
                                  (list t)
                                  (list (build-application no-source
                                          (build-lexical-reference no-source lhs)
                                          '()))
                                  (build-sequence no-source
                                    (list (build-application no-source
                                            (build-lexical-reference no-source lhs)
                                            (list (build-lexical-reference no-source rhs)))
                                          (build-lexical-assignment no-source rhs
                                            (build-lexical-reference no-source t))))))
                              t* lhs* rhs*))))
               (build-application no-source
                 (build-primref no-source 'dynamic-wind)
                 (list (build-lexical-reference no-source swap)
                       (build-lambda no-source '()
                         (chi-internal (cons b b*) r mr))
                       (build-lexical-reference no-source swap))))))))))
  
  (define foreign-call-transformer
    (lambda (e r mr)
      (syntax-match e ()
        ((_ name arg* ...)
         (build-foreign-call no-source
           (chi-expr name r mr)
           (chi-expr* arg* r mr))))))

  ;; p in pattern:                        matches:
  ;;   ()                                 empty list
  ;;   _                                  anything (no binding created)
  ;;   any                                anything
  ;;   (p1 . p2)                          pair
  ;;   #(free-id <key>)                   <key> with free-identifier=?
  ;;   each-any                           any proper list
  ;;   #(each p)                          (p*)
  ;;   #(each+ p1 (p2_1 ... p2_n) p3)      (p1* (p2_n ... p2_1) . p3)
  ;;   #(vector p)                        #(x ...) if p matches (x ...)
  ;;   #(atom <object>)                   <object> with "equal?"
  (define convert-pattern
   ; returns syntax-dispatch pattern & ids
    (lambda (pattern keys)
      (define cvt*
        (lambda (p* n ids)
          (if (null? p*)
              (values '() ids)
              (let-values (((y ids) (cvt* (cdr p*) n ids)))
                (let-values (((x ids) (cvt (car p*) n ids)))
                  (values (cons x y) ids))))))
      (define cvt
        (lambda (p n ids)
          (syntax-match p ()
            (id (id? id)
             (cond
               ((bound-id-member? p keys)
                (values `#(free-id ,p) ids))
               ((free-id=? p (scheme-stx '_))
                (values '_ ids))
               (else (values 'any (cons (cons p n) ids)))))
            ((p dots) (ellipsis? dots)
             (let-values (((p ids) (cvt p (+ n 1) ids)))
               (values
                 (if (eq? p 'any) 'each-any `#(each ,p))
                 ids)))
            ((x dots ys ... . z) (ellipsis? dots)
             (let-values (((z ids) (cvt z n ids)))
               (let-values (((ys ids) (cvt* ys n ids)))
                 (let-values (((x ids) (cvt x (+ n 1) ids)))
                   (values `#(each+ ,x ,(reverse ys) ,z) ids)))))
            ((x . y)
             (let-values (((y ids) (cvt y n ids)))
               (let-values (((x ids) (cvt x n ids)))
                 (values (cons x y) ids))))
            (() (values '() ids))
            (#(p ...) (not (stx? p))
             (let-values (((p ids) (cvt p n ids)))
               (values `#(vector ,p) ids)))
            (datum
             (values `#(atom ,(stx->datum datum)) ids)))))
      (cvt pattern 0 '())))

  (define syntax-dispatch
    (lambda (e p)
      (define stx^
        (lambda (e m* s*)
          (if (and (null? m*) (null? s*))
              e
              (mkstx e m* s*))))
      (define match-each
        (lambda (e p m* s*)
          (cond
            ((pair? e)
             (let ((first (match (car e) p m* s* '())))
               (and first
                    (let ((rest (match-each (cdr e) p m* s*)))
                      (and rest (cons first rest))))))
            ((null? e) '())
            ((stx? e)
             (let-values (((m* s*) (join-wraps m* s* e)))
               (match-each (stx-expr e) p m* s*)))
            (else #f))))
      (define match-each+
        (lambda (e x-pat y-pat z-pat m* s* r)
          (let f ((e e) (m* m*) (s* s*))
            (cond
              ((pair? e)
               (let-values (((xr* y-pat r) (f (cdr e) m* s*)))
                 (if r
                     (if (null? y-pat)
                         (let ((xr (match (car e) x-pat m* s* '())))
                           (if xr
                               (values (cons xr xr*) y-pat r)
                               (values #f #f #f)))
                         (values
                           '()
                           (cdr y-pat)
                           (match (car e) (car y-pat) m* s* r)))
                     (values #f #f #f))))
              ((stx? e)
               (let-values (((m* s*) (join-wraps m* s* e)))
                 (f (stx-expr e) m* s*)))
              (else (values '() y-pat (match e z-pat m* s* r)))))))
      (define match-each-any
        (lambda (e m* s*)
          (cond
            ((pair? e)
             (let ((l (match-each-any (cdr e) m* s*)))
               (and l (cons (stx^ (car e) m* s*) l))))
            ((null? e) '())
            ((stx? e)
             (let-values (((m* s*) (join-wraps m* s* e)))
               (match-each-any (stx-expr e) m* s*)))
            (else #f))))
      (define match-empty
        (lambda (p r)
          (cond
            ((null? p) r)
            ((eq? p '_) r)
            ((eq? p 'any) (cons '() r))
            ((pair? p) (match-empty (car p) (match-empty (cdr p) r)))
            ((eq? p 'each-any) (cons '() r))
            (else
             (case (vector-ref p 0)
               ((each) (match-empty (vector-ref p 1) r))
               ((each+)
                (match-empty
                  (vector-ref p 1)
                  (match-empty
                    (reverse (vector-ref p 2))
                    (match-empty (vector-ref p 3) r))))
               ((free-id atom) r)
               ((vector) (match-empty (vector-ref p 1) r))
               (else (error 'syntax-dispatch "invalid pattern" p)))))))
      (define combine
        (lambda (r* r)
          (if (null? (car r*))
              r
              (cons (map car r*) (combine (map cdr r*) r)))))
      (define match*
        (lambda (e p m* s* r)
          (cond
            ((null? p) (and (null? e) r))
            ((pair? p)
             (and (pair? e)
                  (match (car e) (car p) m* s*
                    (match (cdr e) (cdr p) m* s* r))))
            ((eq? p 'each-any)
             (let ((l (match-each-any e m* s*))) (and l (cons l r))))
            (else
             (case (vector-ref p 0)
               ((each)
                (if (null? e)
                    (match-empty (vector-ref p 1) r)
                    (let ((r* (match-each e (vector-ref p 1) m* s*)))
                      (and r* (combine r* r)))))
               ((free-id)
                (and (symbol? e)
                     (free-id=? (stx^ e m* s*) (vector-ref p 1))
                     r))
               ((each+)
                (let-values (((xr* y-pat r)
                              (match-each+ e (vector-ref p 1)
                                (vector-ref p 2) (vector-ref p 3) m* s* r)))
                  (and r
                       (null? y-pat)
                       (if (null? xr*)
                           (match-empty (vector-ref p 1) r)
                           (combine xr* r)))))
               ((atom) (and (equal? (vector-ref p 1) (strip e m*)) r))
               ((vector)
                (and (vector? e)
                     (match (vector->list e) (vector-ref p 1) m* s* r)))
               (else (error 'syntax-dispatch "invalid pattern" p)))))))
      (define match
        (lambda (e p m* s* r)
          (cond
            ((not r) #f)
            ((eq? p '_) r)
            ((eq? p 'any) (cons (stx^ e m* s*) r))
            ((stx? e)
             (let-values (((m* s*) (join-wraps m* s* e)))
               (match (stx-expr e) p m* s* r)))
            (else (match* e p m* s* r)))))
      (match e p '() '() '())))
  
  (define ellipsis?
    (lambda (x)
      (and (id? x) (free-id=? x (scheme-stx '...)))))

  (define syntax-case-transformer
    (let ()
      (define build-dispatch-call
        (lambda (pvars expr y r mr)
          (let ((ids (map car pvars))
                (levels (map cdr pvars)))
            (let ((labels (map gen-label ids))
                  (new-vars (map gen-lexical ids)))
              (let ((body
                     (chi-expr
                       (add-subst (make-full-rib ids labels) expr)
                       (append
                         (map (lambda (label var level)
                                (cons label (make-binding 'syntax (cons var level))))
                              labels new-vars (map cdr pvars))
                         r)
                       mr)))
                (build-application no-source
                  (build-primref no-source 'apply)
                  (list (build-lambda no-source new-vars body) y)))))))
      (define invalid-ids-error
        (lambda (id* e class)
          (let find ((id* id*) (ok* '()))
            (if (null? id*)
                (stx-error e) ; shouldn't happen
                (if (id? (car id*))
                    (if (bound-id-member? (car id*) ok*)
                        (syntax-error (car id*) "duplicate " class)
                        (find (cdr id*) (cons (car id*) ok*)))
                    (syntax-error (car id*) "invalid " class))))))
      (define gen-clause
        (lambda (x keys clauses r mr pat fender expr)
          (let-values (((p pvars) (convert-pattern pat keys)))
            (cond
              ((not (distinct-bound-ids? (map car pvars)))
               (invalid-ids-error (map car pvars) pat "pattern variable"))
              ((not (for-all (lambda (x) (not (ellipsis? (car x)))) pvars))
               (stx-error pat "misplaced ellipsis in syntax-case pattern"))
              (else
               (let ((y (gen-lexical 'tmp)))
                 (let ((test
                        (cond
                          ((eq? fender #t) y)
                          (else
                           (let ((call
                                  (build-dispatch-call
                                     pvars fender y r mr)))
                             (build-conditional no-source
                                (build-lexical-reference no-source y)
                                call
                                (build-data no-source #f)))))))
                    (let ((conseq
                           (build-dispatch-call pvars expr
                             (build-lexical-reference no-source y)
                             r mr)))
                      (let ((altern
                             (gen-syntax-case x keys clauses r mr)))
                        (build-application no-source
                          (build-lambda no-source (list y)
                            (build-conditional no-source test conseq altern))
                          (list
                            (build-application no-source
                              (build-primref no-source 'syntax-dispatch)
                              (list
                                (build-lexical-reference no-source x)
                                (build-data no-source p))))))))))))))
      (define gen-syntax-case
        (lambda (x keys clauses r mr)
          (if (null? clauses)
              (build-application no-source
                (build-primref no-source 'syntax-error)
                (list (build-lexical-reference no-source x)))
              (syntax-match (car clauses) ()
                ((pat expr)
                 (if (and (id? pat)
                          (not (bound-id-member? pat keys))
                          (not (ellipsis? pat)))
                     (if (free-id=? pat (scheme-stx '_))
                         (chi-expr expr r mr)
                         (let ((lab (gen-label pat))
                               (lex (gen-lexical pat)))
                           (let ((body
                                  (chi-expr
                                    (add-subst (make-full-rib (list pat) (list lab)) expr)
                                    (cons (cons lab (make-binding 'syntax (cons lex 0))) r)
                                    mr)))
                              (build-application no-source
                                (build-lambda no-source (list lex) body)
                                (list (build-lexical-reference no-source x))))))
                     (gen-clause x keys (cdr clauses) r mr pat #t expr)))
                ((pat fender expr)
                 (gen-clause x keys (cdr clauses) r mr pat fender expr))))))
      (lambda (e r mr)
        (syntax-match e ()
          ((_ expr (keys ...) clauses ...)
           (begin
             (unless (for-all (lambda (x) (and (id? x) (not (ellipsis?  x)))) keys)
               (stx-error e "invalid literals"))
             (let ((x (gen-lexical 'tmp)))
               (let ((body (gen-syntax-case x keys clauses r mr)))
                 (build-application no-source
                   (build-lambda no-source (list x) body)
                   (list (chi-expr expr r mr)))))))))))
    (define syntax-transformer
      (let ()
        (define gen-syntax
          (lambda (src e r maps ellipsis? vec?)
            (syntax-match e ()
              (dots (ellipsis? dots)
               (stx-error src "misplaced ellipsis in syntax form"))
              (id (id? id)
               (let* ((label (id->label e))
                      (b (label->binding label r)))
                   (if (eq? (binding-type b) 'syntax)
                       (let-values (((var maps)
                                     (let ((var.lev (binding-value b)))
                                       (gen-ref src (car var.lev) (cdr var.lev) maps))))
                         (values (list 'ref var) maps))
                       (values (list 'quote e) maps))))
              ((dots e) (ellipsis? dots)
               (if vec?
                   (stx-error src "misplaced ellipsis in syntax form")
                   (gen-syntax src e r maps (lambda (x) #f) #f)))
              ((x dots . y) (ellipsis? dots)
               (let f ((y y)
                       (k (lambda (maps)
                            (let-values (((x maps)
                                          (gen-syntax src x r
                                            (cons '() maps) ellipsis? #f)))
                              (if (null? (car maps))
                                  (stx-error src
                                    "extra ellipsis in syntax form")
                                  (values (gen-map x (car maps)) (cdr maps)))))))
                 (syntax-match y ()
                   (() (k maps))
                   ((dots . y) (ellipsis? dots)
                    (f y
                       (lambda (maps)
                         (let-values (((x maps) (k (cons '() maps))))
                           (if (null? (car maps))
                               (stx-error src "extra ellipsis in syntax form")
                               (values (gen-mappend x (car maps)) (cdr maps)))))))
                   (_
                    (let-values (((y maps)
                                  (gen-syntax src y r maps ellipsis? vec?)))
                      (let-values (((x maps) (k maps)))
                        (values (gen-append x y) maps)))))))
              ((x . y)
               (let-values (((xnew maps)
                             (gen-syntax src x r maps ellipsis? #f)))
                 (let-values (((ynew maps)
                               (gen-syntax src y r maps ellipsis? vec?)))
                   (values (gen-cons e x y xnew ynew) maps))))
              (#(ls ...) (not (stx? e))
               (let-values (((lsnew maps)
                             (gen-syntax src ls r maps ellipsis? #t)))
                 (values (gen-vector e ls lsnew) maps)))
              (_ (values `(quote ,e) maps)))))
        (define gen-ref
          (lambda (src var level maps)
            (if (= level 0)
                (values var maps)
                (if (null? maps)
                    (stx-error src "missing ellipsis in syntax form")
                    (let-values (((outer-var outer-maps)
                                  (gen-ref src var (- level 1) (cdr maps))))
                      (cond
                        ((assq outer-var (car maps)) =>
                         (lambda (b) (values (cdr b) maps)))
                        (else
                         (let ((inner-var (gen-lexical 'tmp)))
                           (values
                             inner-var
                             (cons
                               (cons (cons outer-var inner-var) (car maps))
                               outer-maps))))))))))
        (define gen-append
          (lambda (x y)
            (if (equal? y '(quote ())) x `(append ,x ,y))))
        (define gen-mappend
          (lambda (e map-env)
            `(apply (primitive append) ,(gen-map e map-env))))
        (define gen-map
          (lambda (e map-env)
            (let ((formals (map cdr map-env))
                  (actuals (map (lambda (x) `(ref ,(car x))) map-env)))
              (cond
               ; identity map equivalence:
               ; (map (lambda (x) x) y) == y
                ((eq? (car e) 'ref)
                 (car actuals))
               ; eta map equivalence:
               ; (map (lambda (x ...) (f x ...)) y ...) == (map f y ...)
                ((for-all
                   (lambda (x) (and (eq? (car x) 'ref) (memq (cadr x) formals)))
                   (cdr e))
                 (let ((args (map (let ((r (map cons formals actuals)))
                                    (lambda (x) (cdr (assq (cadr x) r))))
                                  (cdr e))))
                   `(map (primitive ,(car e)) . ,args)))
                (else (cons* 'map (list 'lambda formals e) actuals))))))
        (define gen-cons
          (lambda (e x y xnew ynew)
            (case (car ynew)
              ((quote)
               (if (eq? (car xnew) 'quote)
                   (let ((xnew (cadr xnew)) (ynew (cadr ynew)))
                     (if (and (eq? xnew x) (eq? ynew y))
                         `(quote ,e)
                         `(quote ,(cons xnew ynew))))
                   (if (null? (cadr ynew))
                       `(list ,xnew)
                       `(cons ,xnew ,ynew))))
              ((list) `(list ,xnew . ,(cdr ynew)))
              (else `(cons ,xnew ,ynew)))))
        (define gen-vector
          (lambda (e ls lsnew)
            (cond
              ((eq? (car lsnew) 'quote)
               (if (eq? (cadr lsnew) ls)
                   `(quote ,e)
                   `(quote #(,@(cadr lsnew)))))
              ((eq? (car lsnew) 'list)
               `(vector . ,(cdr lsnew)))
              (else `(list->vector ,lsnew)))))
        (define regen
          (lambda (x)
            (case (car x)
              ((ref) (build-lexical-reference no-source (cadr x)))
              ((primitive) (build-primref no-source (cadr x)))
              ((quote) (build-data no-source (cadr x)))
              ((lambda) (build-lambda no-source (cadr x) (regen (caddr x))))
              ((map)
               (let ((ls (map regen (cdr x))))
                 (build-application no-source
                   (build-primref no-source 'map)
                   ls)))
              (else
               (build-application no-source
                 (build-primref no-source (car x))
                 (map regen (cdr x)))))))
        (lambda (e r mr)
          (syntax-match e ()
            ((_ x)
             (let-values (((e maps) (gen-syntax e x r '() ellipsis? #f)))
               (regen e)))))))
  
  (define core-macro-transformer
    (lambda (name)
      (case name
        ((quote)                  quote-transformer)
        ((lambda)                 lambda-transformer)
        ((case-lambda)            case-lambda-transformer)
        ((let-values)             let-values-transformer)
        ((letrec)                 letrec-transformer)
        ((letrec*)                letrec*-transformer)
        ((case)                   case-transformer)
        ((if)                     if-transformer)
        ((when)                   when-transformer)
        ((unless)                 unless-transformer)
        ((parameterize)           parameterize-transformer)
        ((foreign-call)           foreign-call-transformer)
        ((syntax-case)            syntax-case-transformer)
        ((syntax)                 syntax-transformer)
        ((type-descriptor)        type-descriptor-transformer)
        ((record-type-descriptor) record-type-descriptor-transformer)
        ((record-constructor-descriptor) record-constructor-descriptor-transformer)
        (else (error 'macro-transformer "cannot find transformer" name)))))
  
  (define file-options-macro
    (lambda (x)
      (syntax-match x ()
        ((_ opt* ...)
         (and (for-all id? opt*) (file-options-spec (map id->sym opt*)))
         (bless `(quote ,(file-options-spec (map id->sym opt*))))))))

  (define symbol-macro
    (lambda (x set)
      (syntax-match x ()
        ((_ name)
         (and (id? name) (memq (id->sym name) set))
         (bless `(quote ,name))))))

  (define macro-transformer
    (lambda (x)
      (cond
        ((procedure? x) x)
        ((symbol? x)
         (case x
           ((define-record-type)    define-record-type-macro)
           ((define-struct)         define-struct-macro)
           ((include)               include-macro)
           ((cond)                  cond-macro)
           ((let)                   let-macro)
           ((do)                    do-macro)
           ((or)                    or-macro)
           ((and)                   and-macro)
           ((let*)                  let*-macro)
           ((syntax-rules)          syntax-rules-macro)
           ((quasiquote)            quasiquote-macro)
           ((quasisyntax)           quasisyntax-macro)
           ((with-syntax)           with-syntax-macro)
           ((identifier-syntax)     identifier-syntax-macro)
           ((time)                  time-macro)
           ((delay)                 delay-macro)
           ((assert)                assert-macro)
           ((endianness)            endianness-macro)
           ((guard)                 guard-macro)
           ((define-enumeration)    define-enumeration-macro)
           ((trace-lambda)          trace-lambda-macro)
           ((trace-define)          trace-define-macro)
           ((define-condition-type) define-condition-type-macro)
           ((eol-style)
            (lambda (x) 
              (symbol-macro x '(none lf cr crlf nel crnel ls))))
           ((error-handling-mode)         
            (lambda (x) 
              (symbol-macro x '(ignore raise replace))))
           ((buffer-mode)         
            (lambda (x) 
              (symbol-macro x '(none line block))))
           ((file-options)     file-options-macro)
           ((... => _ else unquote unquote-splicing
             unsyntax unsyntax-splicing 
             fields mutable immutable parent protocol
             sealed opaque nongenerative parent-rtd)
            incorrect-usage-macro)
           (else (error 'macro-transformer "invalid macro" x))))
        (else (error 'core-macro-transformer "invalid macro" x)))))
  
  (define (local-macro-transformer x)
    (car x))

  ;;; chi procedures
  (define chi-macro
    (lambda (p e)
      (let ((s ((macro-transformer p) (add-mark anti-mark e))))
        (add-mark (gen-mark) s))))
  
  (define chi-local-macro
    (lambda (p e)
      (let ((s ((local-macro-transformer p) (add-mark anti-mark e))))
        (add-mark (gen-mark) s))))
  
  (define (chi-global-macro p e)
    ;;; FIXME: does not handle macro!?
    (let ((lib (car p))
          (loc (cdr p)))
      (visit-library lib)
      (let ((x (symbol-value loc)))
        (let ((transformer
               (cond
                 ((procedure? x) x)
                 (else (error 'chi-global-macro "not a procedure")))))
          (let ((s (transformer (add-mark anti-mark e))))
            (add-mark (gen-mark) s))))))
  
  (define chi-expr*
    (lambda (e* r mr)
      ;;; expand left to right
      (cond
        ((null? e*) '())
        (else
         (let ((e (chi-expr (car e*) r mr)))
           (cons e (chi-expr* (cdr e*) r mr)))))))

  (define chi-application
    (lambda (e r mr)
      (syntax-match e  ()
        ((rator rands ...)
         (let ((rator (chi-expr rator r mr)))
           (build-application no-source
             rator
             (chi-expr* rands r mr)))))))

  (define chi-expr
    (lambda (e r mr)
      (let-values (((type value kwd) (syntax-type e r)))
        (case type
          ((core-macro)
           (let ((transformer (core-macro-transformer value)))
             (transformer e r mr)))
          ((global)
           (let* ((lib (car value))
                  (loc (cdr value)))
             ((inv-collector) lib)
             (build-global-reference no-source loc)))
          ((core-prim)
           (let ((name value))
             (build-primref no-source name)))
          ((call) (chi-application e r mr))
          ((lexical)
           (let ((lex value))
             (build-lexical-reference no-source lex)))
          ((global-macro global-macro!)
           (chi-expr (chi-global-macro value e) r mr))
          ((local-macro local-macro!) (chi-expr (chi-local-macro value e) r mr))
          ((macro macro!) (chi-expr (chi-macro value e) r mr))
          ((constant)
           (let ((datum value))
             (build-data no-source datum)))
          ((set!) (chi-set! e r mr))
          ((begin)
           (syntax-match e ()
             ((_ x x* ...)
              (build-sequence no-source
                (chi-expr* (cons x x*) r mr)))))
          ((let-syntax letrec-syntax)
           (syntax-match e ()
             ((_ ((xlhs* xrhs*) ...) xbody xbody* ...)
              (unless (valid-bound-ids? xlhs*)
                (stx-error e "invalid identifiers"))
              (let* ((xlab* (map gen-label xlhs*))
                     (xrib (make-full-rib xlhs* xlab*))
                     (xb* (map (lambda (x)
                                (make-eval-transformer
                                  (expand-transformer
                                    (if (eq? type 'let-syntax) x (add-subst xrib x))
                                    mr)))
                              xrhs*)))
                (build-sequence no-source
                  (chi-expr*
                    (map (lambda (x) (add-subst xrib x)) (cons xbody xbody*))
                    (append (map cons xlab* xb*) r)
                    (append (map cons xlab* xb*) mr)))))))
          ((displaced-lexical)
           (stx-error e "identifier out of context"))
          ((syntax) (stx-error e "reference to pattern variable outside a syntax form"))
          ((define define-syntax module import)
           (stx-error e "invalid expression"))
          (else
           ;(error 'chi-expr "invalid type " type (strip e '()))
           (stx-error e "invalid expression"))))))

  (define chi-set!
    (lambda (e r mr)
      (syntax-match e ()
        ((_ x v) (id? x)
         (let-values (((type value kwd) (syntax-type x r)))
           (case type
             ((lexical)
              (build-lexical-assignment no-source
                value
                (chi-expr v r mr)))
             ((global core-prim)
              (stx-error e "cannot modify imported identifier in"))
             ((global-macro!)
              (chi-expr (chi-global-macro value e) r mr))
             ((local-macro!)
              (chi-expr (chi-local-macro value e) r mr))
             (else (stx-error e))))))))
  
  (define chi-lambda-clause
    (lambda (fmls body* r mr)
      (syntax-match fmls ()
        ((x* ...)
         (if (valid-bound-ids? x*)
             (let ((lex* (map gen-lexical x*))
                   (lab* (map gen-label x*)))
               (values
                 lex*
                 (chi-internal
                   (add-subst (make-full-rib x* lab*) body*)
                   (add-lexicals lab* lex* r)
                   mr)))
             (stx-error fmls "invalid fmls")))
        ((x* ... . x)
         (if (valid-bound-ids? (cons x x*))
             (let ((lex* (map gen-lexical x*)) (lab* (map gen-label x*))
                   (lex (gen-lexical x)) (lab (gen-label x)))
               (values
                 (append lex* lex)
                 (chi-internal
                   (add-subst
                     (make-full-rib (cons x x*) (cons lab lab*))
                     body*)
                   (add-lexicals (cons lab lab*) (cons lex lex*) r)
                   mr)))
             (stx-error fmls "invalid fmls")))
        (_ (stx-error fmls "invalid fmls")))))
  
  (define chi-lambda-clause*
    (lambda (fmls* body** r mr)
      (cond
        ((null? fmls*) (values '() '()))
        (else
         (let-values (((a b)
                       (chi-lambda-clause (car fmls*) (car body**) r mr)))
           (let-values (((a* b*)
                         (chi-lambda-clause* (cdr fmls*) (cdr body**) r mr)))
             (values (cons a a*) (cons b b*))))))))

  (define chi-rhs
    (lambda (rhs r mr)
      (case (car rhs)
        ((defun)
         (let ((x (cdr rhs)))
           (let ((fmls (car x)) (body* (cdr x)))
             (let-values (((fmls body)
                           (chi-lambda-clause fmls body* r mr)))
               (build-lambda no-source fmls body)))))
        ((expr)
         (let ((expr (cdr rhs)))
           (chi-expr expr r mr)))
        ((top-expr)
         (let ((expr (cdr rhs)))
           (build-sequence no-source
             (list (chi-expr expr r mr)
                   (build-void)))))
        (else (error 'chi-rhs "invalid rhs" rhs)))))

  (define chi-rhs*
    (lambda (rhs* r mr)
      (let f ((ls rhs*))
        (cond ;;; chi-rhs in order
          ((null? ls) '())
          (else
           (let ((a (chi-rhs (car ls) r mr)))
             (cons a (f (cdr ls)))))))))

  (define find-bound=?
    (lambda (x lhs* rhs*)
      (cond
        ((null? lhs*) #f)
        ((bound-id=? x (car lhs*)) (car rhs*))
        (else (find-bound=? x (cdr lhs*) (cdr rhs*))))))

  (define (find-dups ls)
    (let f ((ls ls) (dups '()))
      (cond
        ((null? ls) dups)
        ((find-bound=? (car ls) (cdr ls) (cdr ls)) =>
         (lambda (x) (f (cdr ls) (cons (list (car ls) x) dups))))
        (else (f (cdr ls) dups)))))
  
  (define chi-internal
    (lambda (e* r mr)
      (let ((rib (make-empty-rib)))
        (let-values (((e* r mr lex* rhs* mod** kwd*)
                      (chi-body* (map (lambda (x) (add-subst rib x))
                                      (syntax->list e*))
                         r mr '() '() '() '() rib #f)))
           (when (null? e*)
             (stx-error e* "no expression in body"))
           (let ((rhs* (chi-rhs* rhs* r mr))
                 (init* (chi-expr* (append (apply append (reverse mod**)) e*) r mr)))
             (build-letrec* no-source
                (reverse lex*) (reverse rhs*)
                (build-sequence no-source init*)))))))

  (define parse-module
    (lambda (e)
      (syntax-match e ()
        ((_ (export* ...) b* ...)
         (begin
           (unless (for-all id? export*)
             (stx-error e "module exports must be identifiers"))
           (values #f export* b*)))
        ((_ name (export* ...) b* ...)
         (begin
           (unless (id? name)
             (stx-error e "module name must be an identifier"))
           (unless (for-all id? export*)
             (stx-error e "module exports must be identifiers"))
           (values name export* b*))))))

  (define chi-internal-module
    (lambda (e r mr lex* rhs* mod** kwd*)
      (let-values (((name exp-id* e*) (parse-module e)))
        (let* ((rib (make-empty-rib))
               (e* (map (lambda (x) (add-subst rib x)) (syntax->list e*))))
          (let-values (((e* r mr lex* rhs* mod** kwd*)
                        (chi-body* e* r mr lex* rhs* mod** kwd* rib #f)))
              (let ((exp-lab*
                     (map (lambda (x)
                             (or (id->label 
                                   (mkstx (id->sym x) (stx-mark* x)
                                     (list rib)))
                                 (stx-error x "cannot find module export")))
                          exp-id*))
                    (mod** (cons e* mod**)))
                (if (not name) ;;; explicit export
                    (values lex* rhs* exp-id* exp-lab* r mr mod** kwd*)
                    (let ((lab (gen-label 'module))
                          (iface (cons exp-id* exp-lab*)))
                      (values lex* rhs*
                              (list name) ;;; FIXME: module cannot
                              (list lab)  ;;;  export itself yet
                              (cons (cons lab (cons '$module iface)) r)
                              (cons (cons lab (cons '$module iface)) mr)
                              mod** kwd*)))))))))

  (define chi-body*
    (lambda (e* r mr lex* rhs* mod** kwd* rib top?)
      (cond
        ((null? e*) (values e* r mr lex* rhs* mod** kwd*))
        (else
         (let ((e (car e*)))
           (let-values (((type value kwd) (syntax-type e r)))
             (let ((kwd* (if (id? kwd) (cons kwd kwd*) kwd*)))
               (case type
                 ((define)
                  (let-values (((id rhs) (parse-define e)))
                    (when (bound-id-member? id kwd*)
                      (stx-error e "cannot redefine keyword"))
                    (let ((lex (gen-lexical id))
                          (lab (gen-label id)))
                      (extend-rib! rib id lab)
                      (chi-body* (cdr e*)
                         (add-lexical lab lex r) mr
                         (cons lex lex*) (cons rhs rhs*)
                         mod** kwd* rib top?))))
                 ((define-syntax)
                  (let-values (((id rhs) (parse-define-syntax e)))
                    (when (bound-id-member? id kwd*)
                      (stx-error e "cannot redefine keyword"))
                    (let ((lab (gen-label id))
                          (expanded-rhs (expand-transformer rhs mr)))
                        (extend-rib! rib id lab)
                        (let ((b (make-eval-transformer expanded-rhs)))
                          (chi-body* (cdr e*)
                             (cons (cons lab b) r) (cons (cons lab b) mr)
                             lex* rhs* mod** kwd* rib top?)))))
                 ((let-syntax letrec-syntax)
                  (syntax-match e ()
                    ((_ ((xlhs* xrhs*) ...) xbody* ...)
                     (unless (valid-bound-ids? xlhs*)
                       (stx-error e "invalid identifiers"))
                     (let* ((xlab* (map gen-label xlhs*))
                            (xrib (make-full-rib xlhs* xlab*))
                            (xb* (map (lambda (x)
                                       (make-eval-transformer
                                         (expand-transformer
                                           (if (eq? type 'let-syntax) x (add-subst xrib x))
                                           mr)))
                                     xrhs*)))
                       (chi-body*
                         (append (map (lambda (x) (add-subst xrib x)) xbody*) (cdr e*))
                         (append (map cons xlab* xb*) r)
                         (append (map cons xlab* xb*) mr)
                         lex* rhs* mod** kwd* rib top?)))))
                 ((begin)
                  (syntax-match e ()
                    ((_ x* ...)
                     (chi-body* (append x* (cdr e*))
                        r mr lex* rhs* mod** kwd* rib top?))))
                 ((global-macro global-macro!)
                  (chi-body*
                     (cons (add-subst rib (chi-global-macro value e)) (cdr e*))
                     r mr lex* rhs* mod** kwd* rib top?))
                 ((local-macro local-macro!)
                  (chi-body*
                     (cons (add-subst rib (chi-local-macro value e)) (cdr e*))
                     r mr lex* rhs* mod** kwd* rib top?))
                 ((macro macro!)
                  (chi-body*
                     (cons (add-subst rib (chi-macro value e)) (cdr e*))
                     r mr lex* rhs* mod** kwd* rib top?))
                 ((module)
                  (let-values (((lex* rhs* m-exp-id* m-exp-lab* r mr mod** kwd*)
                                (chi-internal-module e r mr lex* rhs* mod** kwd*)))
                    (for-each
                      (lambda (id lab) (extend-rib! rib id lab))
                      m-exp-id* m-exp-lab*)
                    (chi-body* (cdr e*) r mr lex* rhs* mod** kwd* rib top?)))
                 ((import)
                  (let ()
                    (define (module-import e r)
                      (syntax-match e ()
                        ((_ id) (id? id)
                         (let-values (((type value kwd) (syntax-type id r)))
                           (case type
                             (($module)
                              (let ((iface value))
                                (let ((id* (car iface)) (lab* (cdr iface)))
                                  (values id* lab*))))
                             (else (stx-error e "invalid import")))))))
                    (let-values (((id* lab*) (module-import e r)))
                      (for-each
                        (lambda (id lab) (extend-rib! rib id lab))
                        id* lab*)))
                  (chi-body* (cdr e*) r mr lex* rhs* mod** kwd* rib top?))
                 (else
                  (if top?
                      (chi-body* (cdr e*) r mr
                          (cons (gen-lexical 'dummy) lex*)
                          (cons (cons 'top-expr e) rhs*)
                          mod** kwd* rib top?)
                      (values e* r mr lex* rhs* mod** kwd*)))))))))))

  (define set-global-macro-binding!
    (lambda (sym loc b)
      (extend-library-subst! (interaction-library) sym loc)
      (extend-library-env! (interaction-library) loc b)))
  
  (define gen-global-macro-binding
    (lambda (id ctxt) (gen-global-var-binding id ctxt)))
  
  (define gen-global-var-binding
    (lambda (id ctxt)
      (let ((label (id->label id)))
        (let ((b (imported-label->binding label)))
          (case (binding-type b)
            ((global)
             (let ((x (binding-value b)))
               (let ((lib (car x)) (loc (cdr x)))
                 (cond
                   ((eq? lib (interaction-library))
                    loc)
                   (else
                    (stx-error ctxt "cannot modify imported binding"))))))
            (else (stx-error ctxt "cannot modify binding in")))))))
  
  (define chi-top-set!
    (lambda (e)
      (syntax-match e ()
        ((_ id rhs) (id? id)
         (let ((loc (gen-global-var-binding id e)))
           (let ((rhs (chi-expr rhs '() '())))
             (values loc rhs)))))))
  
  (define chi-top*
    (lambda (e* init*)
      (cond
        ((null? e*) init*)
        (else
         (let ((e (car e*)))
           (let-values (((type value kwd) (syntax-type e '())))
             (case type
               ((define)
                (let-values (((id rhs) (parse-define e)))
                  (let ((loc (gen-global-var-binding id e)))
                    (let ((rhs (chi-rhs rhs '() '())))
                      (chi-top* (cdr e*) (cons (cons loc rhs) init*))))))
               ((set!)
                (let-values (((loc rhs) (chi-top-set! e)))
                  (chi-top* (cdr e*) (cons (cons loc rhs) init*))))
               ((define-syntax)
                (let-values (((id rhs) (parse-define-syntax e)))
                  (let ((loc (gen-global-macro-binding id e)))
                    (let ((expanded-rhs (expand-transformer rhs '())))
                      (let ((b (make-eval-transformer expanded-rhs)))
                        (set-global-macro-binding! (id->sym id) loc b)
                        (chi-top* (cdr e*) init*))))))
               ((let-syntax letrec-syntax) 
                (error 'chi-top* "not supported yet at top level" type))
               ((begin)
                (syntax-match e ()
                  ((_ x* ...)
                   (chi-top* (append x* (cdr e*)) init*))))
               ((global-macro global-macro!)
                (chi-top* (cons (chi-global-macro value e) (cdr e*)) init*))
               ((local-macro local-macro!)
                (chi-top* (cons (chi-local-macro value e) (cdr e*)) init*))
               ((macro macro!)
                (chi-top* (cons (chi-macro value e) (cdr e*)) init*))
               (else
                (chi-top* (cdr e*)
                   (cons (cons #f (chi-expr e '() '()))
                      init*))))))))))
  
  (define (expand-transformer expr r)
    (let ((rtc (make-collector)))
      (let ((expanded-rhs
             (parameterize ((inv-collector rtc)
                            (vis-collector (lambda (x) (values))))
                 (chi-expr expr r r))))
        (for-each
          (let ((mark-visit (vis-collector)))
            (lambda (x)
              (invoke-library x)
              (mark-visit x)))
          (rtc))
        expanded-rhs)))

  (define (parse-exports exp*)
    (let f ((exp* exp*) (int* '()) (ext* '()))
      (cond
        ((null? exp*)
         (let ((id* (map (lambda (x) (mkstx x top-mark* '())) ext*)))
           (unless (valid-bound-ids? id*)
             (error 'expander "invalid exports" (find-dups id*))))
         (values int* ext*))
        (else
         (syntax-match (car exp*) ()
           ((rename (i* e*) ...)
            (begin
              (unless (and (eq? rename 'rename) (for-all symbol? i*)
                           (for-all symbol? e*))
                (error 'expander "invalid export specifier" (car exp*)))
              (f (cdr exp*) (append i* int*) (append e* ext*))))
           (ie
            (begin
              (unless (symbol? ie) (error 'expander "invalid export" ie))
              (f (cdr exp*) (cons ie int*) (cons ie ext*)))))))))

  ;;; given a library name, like (foo bar (1 2 3)), 
  ;;; returns the identifiers and the version of the library
  ;;; as (foo bar) (1 2 3).  
  (define (parse-library-name spec)
    (define (parse x)
      (syntax-match x ()
        (((v* ...)) 
         (for-all (lambda (x) (and (integer? x) (exact? x))) v*)
         (values '() v*))
        ((x . rest) (symbol? x)
         (let-values (((x* v*) (parse rest)))
           (values (cons x x*) v*)))
        (() (values '() '()))
        (_ (stx-error spec "invalid library name"))))
    (let-values (((name* ver*) (parse spec)))
      (when (null? name*) (stx-error spec "empty library name"))
      (values name* ver*)))

  ;;; given a library form, returns the name part, the export 
  ;;; specs, import specs and the body of the library.  
  (define parse-library
    (lambda (e)
      (syntax-match e ()
        ((library (name* ...)
            (export exp* ...)
            (import imp* ...)
            b* ...)
         (and (eq? export 'export) (eq? import 'import) (eq? library 'library))
         (values name* exp* imp* b*))
        (_ (stx-error e "malformed library")))))

  ;;; given a list of import-specs, return a subst and the list of
  ;;; libraries that were imported.
  ;;; Example: given ((rename (only (foo) x z) (x y)) (only (bar) q))
  ;;; returns: ((z . z$label) (y . x$label) (q . q$label))
  ;;;     and  (#<library (foo)> #<library (bar)>)
  (define (parse-import-spec* imp*)
    (define imp-collector (make-collector))
    (define (merge-substs s subst)
      (define (insert-to-subst a subst)
        (let ((name (car a)) (label (cdr a)))
          (cond
            ((assq name subst) =>
             (lambda (x)
               (cond
                 ((eq? (cdr x) label) subst)
                 (else
                  (error 'import
                     "two imports with different bindings"
                     name)))))
            (else
             (cons a subst)))))
      (cond
        ((null? s) subst)
        (else
         (insert-to-subst (car s)
           (merge-substs (cdr s) subst)))))
    (define (exclude* sym* subst)
      (define (exclude sym subst)
        (cond
          ((null? subst)
           (error 'import "cannot rename unbound identifier" sym))
          ((eq? sym (caar subst))
           (values (cdar subst) (cdr subst)))
          (else
           (let ((a (car subst)))
             (let-values (((old subst) (exclude sym (cdr subst))))
               (values old (cons a subst)))))))
      (cond
        ((null? sym*) (values '() subst))
        (else
         (let-values (((old subst) (exclude (car sym*) subst)))
           (let-values (((old* subst) (exclude* (cdr sym*) subst)))
             (values (cons old old*) subst))))))
    (define (find* sym* subst)
      (map (lambda (x)
             (cond
               ((assq x subst) => cdr)
               (else (error 'import "cannot find identifier" x))))
           sym*))
    (define (rem* sym* subst)
      (let f ((subst subst))
        (cond
          ((null? subst) '())
          ((memq (caar subst) sym*) (f (cdr subst)))
          (else (cons (car subst) (f (cdr subst)))))))
    (define (remove-dups ls)
      (cond
        ((null? ls) '())
        ((memq (car ls) (cdr ls)) (remove-dups (cdr ls)))
        (else (cons (car ls) (remove-dups (cdr ls))))))
    (define (parse-library-name spec)
      (define (subversion? x) 
        (and (integer? x) (exact? x) (>= x 0)))
      (define (subversion-pred x*) 
        (syntax-match x* ()
          (n (subversion? n)
           (lambda (x) (= x n)))
          ((p? sub* ...) (eq? p? 'and)
           (let ((p* (map subversion-pred sub*)))
             (lambda (x) 
               (for-all (lambda (p) (p x)) p*))))
          ((p? sub* ...) (eq? p? 'or)
           (let ((p* (map subversion-pred sub*)))
             (lambda (x) 
               (exists (lambda (p) (p x)) p*))))
          ((p? sub) (eq? p? 'not)
           (let ((p (subversion-pred sub)))
             (lambda (x) 
               (not (p x)))))
          ((p? n) (and (eq? p? '<=) (subversion? n))
           (lambda (x) (<= x n)))
          ((p? n) (and (eq? p? '>=) (subversion? n))
           (lambda (x) (>= x n)))
          (_ (error 'import "invalid sub-version spec" x* spec))))
      (define (version-pred x*)
        (syntax-match x* ()
          (() (lambda (x) #t))
          ((c ver* ...) (eq? c 'and)
           (let ((p* (map version-pred ver*)))
             (lambda (x) 
               (for-all (lambda (p) (p x)) p*))))
          ((c ver* ...) (eq? c 'or)
           (let ((p* (map version-pred ver*)))
             (lambda (x) 
               (exists (lambda (p) (p x)) p*))))
          ((c ver) (eq? c 'not)
           (let ((p (version-pred ver)))
             (lambda (x) (not (p x)))))
          ((sub* ...) 
           (let ((p* (map subversion-pred sub*)))
             (lambda (x) 
               (let f ((p* p*) (x x))
                 (cond
                   ((null? p*) #t)
                   ((null? x) #f)
                   (else 
                    (and ((car p*) (car x)) 
                         (f (cdr p*) (cdr x*)))))))))
          (_ (error 'import "invalid version spec" x* spec))))
      (let f ((x spec))
        (syntax-match x ()
          (((version-spec* ...)) 
           (values '() (version-pred version-spec*)))
          ((x . x*) (symbol? x)
           (let-values (((name pred) (f x*)))
             (values (cons x name) pred)))
          (() (values '() (lambda (x) #t)))
          (_ (stx-error spec "invalid import spec")))))
    (define (get-import spec)
      (syntax-match spec ()
        ((rename isp (old* new*) ...) 
         (and (eq? rename 'rename) (for-all symbol? old*) (for-all symbol? new*))
         (let ((subst (get-import isp)))
           (let ((old-label* (find* old* subst)))
             (let ((subst (rem* old* subst)))
               ;;; FIXME: make sure map is valid
               (merge-substs (map cons new* old-label*) subst)))))
        ((except isp sym* ...) 
         (and (eq? except 'except) (for-all symbol? sym*))
         (let ((subst (get-import isp)))
           (rem* sym* subst)))
        ((only isp sym* ...)
         (and (eq? only 'only) (for-all symbol? sym*))
         (let ((subst (get-import isp)))
           (let ((sym* (remove-dups sym*)))
             (let ((lab* (find* sym* subst)))
               (map cons sym* lab*)))))
        ((prefix isp p) 
         (and (eq? prefix 'prefix) (symbol? p))
         (let ((subst (get-import isp)))
           (map
             (lambda (x)
               (cons
                 (string->symbol
                   (string-append 
                     (symbol->string p)
                     (symbol->string (car x))))
                 (cdr x)))
             subst)))
        ((library (spec* ...)) (eq? library 'library)
         ;;; FIXME: versioning stuff
         (let-values (((name pred) (parse-library-name spec*)))
           (when (null? name) 
             (error 'import "empty library name" spec*))
           (let ((lib (find-library-by-name name)))
             (unless lib
               (error 'import 
                  "cannot find library with required name"
                  name))
             (unless (pred (library-version lib))
               (error 'import 
                  "library does not satisfy version specification"
                  lib
                  spec*))
             (imp-collector lib)
             (library-subst lib))))
        ((x x* ...)
         (not (memq x '(rename except only prefix library)))
         (get-import `(library (,x . ,x*))))
        (spec (error 'import "invalid import spec" spec))))
    (let f ((imp* imp*) (subst '()))
      (cond
        ((null? imp*) (values subst (imp-collector)))
        (else
         (f (cdr imp*) (merge-substs (get-import (car imp*)) subst))))))

  ;;; a top rib is constructed as follows:
  ;;; given a subst: name* -> label*,
  ;;; generate a rib containing:
  ;;;  - name* as the rib-sym*,
  ;;;  - a list of top-mark* as the rib-mark**
  ;;;  - label* as the rib-label*
  ;;; so, a name in a top rib maps to its label if and only if
  ;;; its set of marks is top-mark*.
  (define (make-top-rib subst)
    (let ((rib (make-empty-rib)))
      (for-each
        (lambda (x)
          (let ((name (car x)) (label (cdr x)))
            (extend-rib! rib (mkstx name top-mark* '()) label)))
        subst)
      rib))

  (define (make-collector)
    (let ((ls '()))
      (case-lambda
        (() ls)
        ((x) (set! ls (set-cons x ls))))))

  (define inv-collector
    (make-parameter
      (lambda args
        (error 'inv-collector "not initialized"))
      (lambda (x)
        (unless (procedure? x)
          (error 'inv-collector "not a procedure" x))
        x)))
  
  (define vis-collector
    (make-parameter
      (lambda args
        (error 'vis-collector "not initialized"))
      (lambda (x)
        (unless (procedure? x)
          (error 'vis-collector "not a procedure" x))
        x)))
  
  (define chi-library-internal
    (lambda (e* rib top?)
      (let-values (((e* r mr lex* rhs* mod** _kwd*)
                    (chi-body* e* '() '() '() '() '() '() rib top?)))
        (values (append (apply append (reverse mod**)) e*)
           r mr (reverse lex*) (reverse rhs*)))))
  
  (define library-body-expander
    (lambda (exp* imp* b*)
      (let-values (((exp-int* exp-ext*) (parse-exports exp*))
                   ((subst imp*) (parse-import-spec* imp*)))
        (let ((rib (make-top-rib subst)))
          (let ((b* (map (lambda (x) (mkstx x top-mark* (list rib))) b*))
                (rtc (make-collector))
                (vtc (make-collector)))
            (parameterize ((inv-collector rtc)
                           (vis-collector vtc))
              (let-values (((init* r mr lex* rhs*)
                            (chi-library-internal b* rib #f)))
                (seal-rib! rib)
                (let ((rhs* (chi-rhs* rhs* r mr))
                      (init* (chi-expr* init* r mr)))
                  (unseal-rib! rib)
                  (let ((export-subst (make-export-subst exp-int* exp-ext* rib)))
                    (let-values (((export-env global* macro*)
                                  (make-export-env/macros r)))
                      (let ((invoke-body
                             (build-letrec* no-source lex* rhs* 
                                (build-exports global* init*)))
                            (invoke-definitions 
                             (map build-global-define (map cdr global*))))
                        (values
                          imp* (rtc) (vtc)
                          (build-sequence no-source 
                            (append invoke-definitions
                              (list invoke-body)))
                          macro* export-subst export-env))))))))))))

  (define core-library-expander
    (lambda (e)
      (let-values (((name* exp* imp* b*) (parse-library e)))
        (let-values (((name ver) (parse-library-name name*)))
          (let-values (((imp* invoke-req* visit-req* invoke-code
                              visit-code export-subst export-env)
                        (library-body-expander exp* imp* b*)))
             (values name ver imp* invoke-req* visit-req* 
                     invoke-code visit-code export-subst
                     export-env))))))
  
  (define (parse-top-level-program e*)
    (syntax-match e* ()
      (((import imp* ...) b* ...) (eq? import 'import)
       (values imp* b*))
      (_ (error "invalid syntax of top-level program"))))

  (define top-level-expander
    (lambda (e*)
      (let-values (((imp* b*) (parse-top-level-program e*)))
          (let-values (((imp* invoke-req* visit-req* invoke-code
                         visit-code export-subst export-env)
                        (library-body-expander '() imp* b*)))
            (values invoke-req* invoke-code)))))

  ;;; An env record encapsulates a substitution and a set of
  ;;; libraries.
  (define-record env (subst imp*)
    (lambda (x p)
      (unless (env? x)
        (error 'record-type-printer "not an environment"))
      (display "#<environment>" p)))
  
  (define environment?
    (lambda (x) (env? x)))
  
  ;;; This is R6RS's environment.  It parses the import specs 
  ;;; and constructs an env record that can be used later by 
  ;;; eval and/or expand.
  (define environment
    (lambda imp*
      (let-values (((subst imp*) (parse-import-spec* imp*)))
        (make-env subst imp*))))
  
  ;;; R6RS's null-environment and scheme-report-environment are
  ;;; constructed simply using the corresponding libraries.
  (define (null-environment n)
    (unless (eqv? n 5)
      (error 'null-environment "not 5" n))
    (environment '(psyntax null-environment-5)))
  (define (scheme-report-environment n)
    (unless (eqv? n 5)
      (error 'scheme-report-environment "not 5" n))
    (environment '(psyntax scheme-report-environment-5)))

  ;;; The expand procedure is the interface to the internal expression
  ;;; expander (chi-expr).   It takes an expression and an environment.
  ;;; It returns two values: The resulting core-expression and a list of
  ;;; libraries that must be invoked before evaluating the core expr.
  (define expand
    (lambda (x env)
      (unless (env? env)
        (error 'expand "not an environment" env))
      (let ((subst (env-subst env)))
        (let ((rib (make-top-rib subst)))
          (let ((x (mkstx x top-mark* (list rib)))
                (rtc (make-collector))
                (vtc (make-collector)))
              (let ((x
                     (parameterize ((inv-collector rtc)
                                    (vis-collector vtc))
                        (chi-expr x '() '()))))
                (seal-rib! rib)
                (values x (rtc))))))))

  ;;; This is R6RS's eval.  It takes an expression and an environment,
  ;;; expands the expression, invokes its invoke-required libraries and
  ;;; evaluates its expanded core form.
  (define eval
    (lambda (x env)
      (unless (env? env)
        (error 'eval "not an environment" env))
      (let-values (((x invoke-req*) (expand x env)))
        (for-each invoke-library invoke-req*)
        (eval-core (expanded->core x)))))

  ;;; Given a (library . _) s-expression, library-expander expands
  ;;; it to core-form, registers it with the library manager, and
  ;;; returns its invoke-code, visit-code, subst and env.
  (define (library-expander x)
    (define (build-visit-code macro*)
      (if (null? macro*)
          (build-void)
          (build-sequence no-source
            (map (lambda (x)
                   (let ((loc (car x)) (src (cddr x)))
                     (build-global-assignment no-source loc src)))
                 macro*))))
    (define (visit! macro*)
      (for-each (lambda (x)
                  (let ((loc (car x)) (proc (cadr x)))
                    (set-symbol-value! loc proc)))
                macro*))
    (let-values (((name ver imp* inv* vis* invoke-code macro* export-subst export-env)
                  (core-library-expander x)))
      (let ((id (gensym))
            (name name)
            (ver ver)  ;;; FIXME
            (imp* (map library-spec imp*))
            (vis* (map library-spec vis*))
            (inv* (map library-spec inv*)))
        (install-library id name ver
           imp* vis* inv* export-subst export-env
           (lambda () (visit! macro*))
           (lambda () (eval-core (expanded->core invoke-code)))
           #t)
        (values invoke-code
                (build-visit-code macro*)
                export-subst export-env))))

  ;;; when bootstrapping the system, visit-code is not (and cannot
  ;;; be) be used in the "next" system.  So, we drop it.
  (define (boot-library-expand x)
    (let-values (((invoke-code visit-code export-subst export-env)
                  (library-expander x)))
      (values invoke-code export-subst export-env)))
  
  (define (rev-map-append f ls ac)
    (cond
      ((null? ls) ac)
      (else
       (rev-map-append f (cdr ls)
          (cons (f (car ls)) ac)))))
  
  (define build-exports
    (lambda (lex*+loc* init*)
      (build-sequence no-source
        (cons (build-void)
          (rev-map-append
            (lambda (x)
              (build-global-assignment no-source (cdr x) (car x)))
            lex*+loc*
            init*)))))
  
  (define (make-export-subst int* ext* rib)
    (map
      (lambda (int ext)
        (let* ((id (mkstx int top-mark* (list rib)))
               (label (id->label id)))
          (unless label
            (stx-error id "cannot export unbound identifier"))
          (cons ext label)))
      int* ext*))
  
  (define (make-export-env/macros r)
    (let f ((r r) (env '()) (global* '()) (macro* '()))
      (cond
        ((null? r) (values env global* macro*))
        (else
         (let ((x (car r)))
           (let ((label (car x)) (b (cdr x)))
             (case (binding-type b)
               ((lexical)
                (let ((loc (gen-global (binding-value b))))
                  (f (cdr r)
                     (cons (cons* label 'global loc) env)
                     (cons (cons (binding-value b) loc) global*)
                     macro*)))
               ((local-macro)
                (let ((loc (gensym)))
                  (f (cdr r)
                     (cons (cons* label 'global-macro loc) env)
                     global*
                     (cons (cons loc (binding-value b)) macro*))))
               ((local-macro!)
                (let ((loc (gensym)))
                  (f (cdr r)
                     (cons (cons* label 'global-macro! loc) env)
                     global*
                     (cons (cons loc (binding-value b)) macro*))))
               (($rtd $module) (f (cdr r) (cons x env) global* macro*))
               (else
                (error 'expander "BUG: do not know how to export"
                       (binding-type b) (binding-value b))))))))))
  
  (define generate-temporaries
    (lambda (ls)
      (syntax-match ls ()
        ((ls ...)
         (map (lambda (x) (make-stx (gensym 't) top-mark* '())) ls))
        (_ 
         (error 'generate-temporaries "not a list")))))
  
  (define free-identifier=?
    (lambda (x y)
      (if (id? x)
          (if (id? y)
              (free-id=? x y)
              (error 'free-identifier=? "not an identifier" y))
          (error 'free-identifier=? "not an identifier" x))))
  
  (define bound-identifier=?
    (lambda (x y)
      (if (id? x)
          (if (id? y)
              (bound-id=? x y)
              (error 'bound-identifier=? "not an identifier" y))
          (error 'bound-identifier=? "not an identifier" x))))
  
  (define syntax-error
    (lambda (x . args)
      (unless (for-all string? args)
        (error 'syntax-error "invalid argument" args))
      (error 'expander "invalid syntax"
          (stx->datum x) (apply string-append args))))
  
  (define identifier? (lambda (x) (id? x)))
  
  (define datum->syntax
    (lambda (id datum)
      (if (id? id)
          (datum->stx id datum)
          (error 'datum->syntax "not an identifier" id))))
  
  (define syntax->datum
    (lambda (x) (stx->datum x)))

  (define eval-r6rs-top-level
    (lambda (x*)
      (let-values (((lib* invoke-code) (top-level-expander x*)))
        (for-each invoke-library lib*)
        (eval-core (expanded->core invoke-code)))))

  ;;; The interaction-library is a parameter that is either #f 
  ;;; (the default, for r6rs scripts) or set to an extensible library
  ;;; that serves as the base for an r5rs-like top-level environment.
  ;;; The identifiers in the top-level library are copied on demand from
  ;;; the (ikarus) library which contains all the public bindings of the
  ;;; system.

  (define interaction-library (make-parameter #f))

  (define (interaction-sym->label sym) 
    (cond
      ((interaction-library) =>
       (lambda (lib)
         (cond
           ((assq sym (library-subst lib)) => cdr)
           (else
            (let ((subst 
                   (if (library-exists? '(ikarus))
                       (library-subst (find-library-by-name '(ikarus)))
                       '())))
              (cond
                ((assq sym subst) =>
                 (lambda (sym/lab)
                   (let ((label (cdr sym/lab)))
                     (extend-library-subst! lib sym label)
                     label)))
                (else
                 (let ((label (gen-label sym)))
                   (extend-library-subst! lib sym label)
                   (extend-library-env! lib label
                     (cons 'global (cons lib (gen-global sym))))
                   label))))))))
      (else #f)))

  (define eval-top-level
    (lambda (x)
      (define (eval-binding x)
        (let ((loc (car x)) (expr (cdr x)))
          (cond
            (loc (set-symbol-value! loc (eval-core (expanded->core expr))))
            (else (eval-core (expanded->core expr))))))
      (let ((rtc (make-collector))
            (vtc (make-collector)))
        (let ((init*
               (parameterize ((inv-collector rtc)
                              (vis-collector vtc)
                              (interaction-library
                               (find-library-by-name '(ikarus interaction))))
                 (chi-top* (list (mkstx x top-mark* '())) '()))))
          (for-each invoke-library (rtc))
          (unless (null? init*)
            (for-each eval-binding (reverse (cdr init*)))
            (eval-binding (car init*)))))))

  ;;; register the expander with the library manager
  (current-library-expander library-expander))



