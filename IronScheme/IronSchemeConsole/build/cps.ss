;;; -*- Mode: Scheme -*-

;;;; Redex-Free, General CPS Generator

;;; Copyright (c) 2008, Taylor R. Campbell
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;;
;;; * Redistributions in binary form must reproduce the above copyright
;;;   notice, this list of conditions and the following disclaimer in
;;;   the documentation and/or other materials provided with the
;;;   distribution.
;;;
;;; * Neither the names of the authors nor the names of contributors
;;;   may be used to endorse or promote products derived from this
;;;   software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; This file implements a simple, redex-free, and general
;;; continuation-passing style code generator.  The representation of
;;; the output is isolated from the details of the CPS, and the code
;;; generation routines are provided for use by a driver.  At the
;;; bottom of the file there is a sample output format, as well as a
;;; sample driver written with Alex Shinn's MATCH macro for matching
;;; patterns of S-expressions; the macro's definition is available at
;;; <http://synthcode.com/scheme/match.scm>.
;;;
;;; One ostensibly internal routine bears elaboration:
;;; CPS/GENERATE-SUBPROBLEMS.  This is what orders, or doesn't order,
;;; the resulting program.  The definition supplied here orders
;;; subproblems left-to-right.  CPS/GENERATE-COMBINATION uses whatever
;;; order CPS/GENERATE-SUBPROBLEMS chooses for the *reversed* list of
;;; operator and operands.  If your output format can support order-
;;; independent data structures, here called parallel fork and parallel
;;; join nodes, you should use the alternative definition that does not
;;; commit to a particular order of evaluation of subproblems.  Also,
;;; CPS/GENERATE-SUBPROBLEMS may be useful for generating non-standard
;;; combinations, such as combinations whose operators are known and
;;; can benefit from slight variation in their generation.
;;;
;;; This file is subject to change.  Specifically, it would be nice to
;;; supply auxiliary information to the generators, so that we can, for
;;; instance, annotate the output terms with source history, without
;;; dramatically affecting the structure of the generator.  I have not
;;; yet conceived the best way to go about this.  Don't stay tuned,
;;; however, for it is unlikely that I shall find time to think
;;; seriously about such questions.

;;;; CPS Generation

;;; The procedures CPS/GENERATE-* return code generator procedures that
;;; accept continuators as arguments.  A continuator is a description
;;; of what to do next in the program; see below for details.

(library (ironscheme cps)
  (export 
    expand-boot-cps
    convert->cps)
  (import 
    (except (ironscheme) expand-boot-cps convert->cps)
    (ironscheme clr))

(define (cps/generate-reference variable)
  (cps/generate-operand (make-reference variable)))

(define (cps/generate-literal datum)
  (cps/generate-operand (make-literal datum)))

(define (cps/generate-abstraction bvl body-generator)
  (cps/generate-operand
   (make-user-abstraction
    bvl
    (lambda (continuation-variable)
      (body-generator (variable-continuator continuation-variable))))))
      
(define (cps/generate-abstractions bvls body-generators)
  (cps/generate-operand
   (make-user-abstractions
    bvls
    (map 
      (lambda (body-generator)
        (lambda (continuation-variable)
          (body-generator (variable-continuator continuation-variable))))
      body-generators))))      

(define (cps/generate-operand operand)
  (lambda (continuator)
    (continue-with-operand continuator operand)))

(define (cps/generate-conditional condition-generator
                                  consequent-generator
                                  alternative-generator)
  (lambda (continuator)
    (define (if-variable continuation-variable)
      ;; If CONTINUATOR is a variable continuator, we could avoid
      ;; making another and rely having <v> = (REIFY-CONTINUATION*
      ;; (VARIABLE-CONTINUATOR <v>) VALUES ...).  But this is easier.
      (let ((continuator* (variable-continuator continuation-variable)))
        (condition-generator
         (conditional-continuator
          (lambda (condition)
            (make-conditional condition
                              (consequent-generator continuator*)
                                (alternative-generator continuator*)))))))
    (reify-continuation*
     continuator
     if-variable
     (lambda (continuation-abstraction)
       (make-conditional-join continuation-abstraction if-variable)))))

(define (cps/generate-recursive-bind variables generators body-generator)
  (lambda (continuator)
    (make-recursive-bind
     variables
     (map (lambda (generator)
            (lambda (continuation-variable)
              (generator
               ;; Perhaps there should be a new kind of continuator for
               ;; the expressions of recursive bindings.
               (variable-continuator continuation-variable))))
          generators)
     (body-generator (reified-continuator continuator)))))

(define (cps/generate-recursive*-bind variables generators body-generator)
  (lambda (continuator)
    (make-recursive*-bind
     variables
     (map (lambda (generator)
            (lambda (continuation-variable)
              (generator
               ;; Perhaps there should be a new kind of continuator for
               ;; the expressions of recursive bindings.
               (variable-continuator continuation-variable))))
          generators)
     (body-generator (reified-continuator continuator)))))
     
(define (cps/generate-recursive*-library-bind name variables variables* generators body-generator)
  (lambda (continuator)
    (make-recursive*-library-bind
     name
     variables
     variables*
     (map (lambda (generator)
            (lambda (continuation-variable)
              (generator
               ;; Perhaps there should be a new kind of continuator for
               ;; the expressions of recursive bindings.
               (variable-continuator continuation-variable))))
          generators)
     (body-generator (reified-continuator continuator)))))     
     
(define (cps/generate-sequence generators)
  (lambda (continuator)
    (let recur ((generators generators))
      ((car generators)
       (let ((generators (cdr generators)))
         (if (pair? generators)
             (effect-continuator (lambda () (recur generators)))
             continuator))))))
             
(define (cps/generate-assignment location expression-generator) 
  (lambda (continuator) 
    (expression-generator 
      (operand-continuator 
        (lambda (operand) 
          (make-assignment location 
                           operand 
                           (continue-with-operand continuator '(void))))))))    
                           
(define (cps/generate-primitive operator operand-generators) 
  (lambda (continuator) 
    (continue-with-operand continuator 
      (cons operator 
            (map
              (lambda (generator)
                (lambda (continuation-variable)
                  (generator 
                    (variable-continuator continuation-variable))))
              operand-generators)))))
                           
;;;; Subproblems and Combinations

(define (cps/generate-combination operator-generator operand-generators)
  (cps/generate-subproblems
   (reverse (cons operator-generator operand-generators))
   (lambda (operands continuator)
     (make-combination (car operands)
                       (reify-continuation continuator)
                       (cdr operands)))))

;;; This version linearizes to left-to-right order.

(define (cps/generate-subproblems generators combiner)
  (lambda (continuator)
    (let recur ((generators generators) (operands '()))
      (if (pair? generators)
          ((car generators)
           (let ((generators (cdr generators)))
             (operand-continuator
              (lambda (operand)
                (recur generators (cons operand operands))))))
          (combiner operands continuator)))))

;;; If we have separate parallel-fork and parallel-join nodes (which
;;; ought to be named differently: there isn't real parallelism here;
;;; there is only independence of order), then we can do a better job
;;; by representing the subproblems in parallel with a fork node
;;; branching out to the subproblems and a join node combining their
;;; results, without losing the continuation-passing structure.

;; (define (cps/generate-subproblems generators combiner)
;;   (lambda (continuator)
;;     (let ((join (make-parallel-join))
;;           (operands '()))
;;       ;; We can't use MAP here because we must go through in order, so
;;       ;; that the update of OPERANDS will be in order.
;;       (let loop ((generators generators) (subproblems '()))
;;         (if (pair? generators)
;;             (loop (cdr generators)
;;                   (cons ((car generators)
;;                          (reified-continuator
;;                           (operand-continuator
;;                            (lambda (operand)
;;                              (set! operands (cons operand operands))
;;                              join))))
;;                         subproblems))
;;             (begin
;;               (set-parallel-join.body! join (combiner operands continuator))
;;               ;; Reverse to preserve the user's order, for display.
;;               (make-parallel-fork (reverse subproblems) join)))))))

;;;; Continuators

;;; A continuator is a meta-description of where the object-program
;;; will proceed next.  We don't always want to generate a full-blown
;;; lambda node for each continuation in the object-program; for
;;; example, if we are generating code for a simple operand, we can
;;; just substitute it directly into wherever the object-program will
;;; continue, such as in an argument position.

;;; In an operand continuator, GENERATOR expects a simple operand.

(define (operand-continuator generator)
  (lambda (if-operand if-effect if-conditional if-variable if-inline)
    if-effect if-conditional if-variable if-inline ;ignore
    (if-operand generator)))

;;; In an effect continuator, GENERATOR takes zero arguments; we care
;;; only about effects, not values.

(define (effect-continuator generator)
  (lambda (if-operand if-effect if-conditional if-variable if-inline)
    if-operand if-conditional if-variable if-inline ;ignore
    (if-effect generator)))

;;; A conditional continuator is like an operand continuator except
;;; that `conditions' may be somewhat different from operands.  For
;;; instance, the client may require conditions to be combinations, and
;;; change (IF X ...) into (IF (TRUE? X) ...).

(define (conditional-continuator generator)
  (lambda (if-operand if-effect if-conditional if-variable if-inline)
    if-operand if-effect if-variable if-inline ;ignore
    (if-conditional generator)))

;;; If we have only a name for the object-continuation, or an
;;; expression, then we use variable or inline continuators.

(define (variable-continuator variable)
  (lambda (if-operand if-effect if-conditional if-variable if-inline)
    if-operand if-effect if-variable if-inline ;ignore
    (if-variable variable)))

(define (inline-continuator continuation)
  (lambda (if-operand if-effect if-conditional if-variable if-inline)
    if-operand if-effect if-conditional if-variable ;ignore
    (if-inline continuation)))

(define (reified-continuator continuator)
  (lambda (if-operand if-effect if-conditional if-variable if-inline)
    if-operand if-effect if-conditional ;ignore
    (reify-continuation* continuator if-variable if-inline)))

;;; The simplest way to use a continuator is when we have a single
;;; operand whose value we want to supply: a literal datum, a variable
;;; reference, or a lambda abstraction.

(define (continue-with-operand continuator operand)
  (continuator (lambda (generator) (generator operand))
               (lambda (generator) (generator))
               (lambda (generator) (generator (make-truth-test operand)))
               (lambda (continuation-variable)
                 (make-return (make-reference continuation-variable)
                              (list operand)))
               (lambda (continuation-abstraction)
                 (make-return continuation-abstraction (list operand)))))

;;;;; Reifying Continuations

;;; When we really do need to generate a lambda node, such as when
;;; generating combinations or conditionals, we reify the continuation
;;; denoted by a continuator.  The simple way (without the asterisk) is
;;; just to generate an object-expression for the continuation; this is
;;; what combinations need.  The complex way (with the asterisk) lets
;;; the caller do something different depending on whether we already
;;; have a name for the continuation, or whether we must express it
;;; with a lambda node.  The generation of code for conditionals uses
;;; this to generate a LET for a join point only if necessary.

(define (reify-continuation continuator)
  (continuator operand-continuation-reifier
               effect-continuation-reifier
               conditional-continuation-reifier
               (lambda (variable) (make-reference variable))
               (lambda (continuation) continuation)))

(define (reify-continuation* continuator if-variable if-inline)
  (continuator (operand-continuation-reifier* if-inline)
               (effect-continuation-reifier* if-inline)
               (conditional-continuation-reifier* if-inline)
               if-variable
               if-inline))

;;; (LAMBDA (V) ...)

(define (operand-continuation-reifier* receiver)
  (lambda (generator)
    (receiver
     (let ((variable (generate-variable 'V)))
       (make-continuation-abstraction
        (list variable)
        (generator (make-reference variable)))))))

(define operand-continuation-reifier
  (operand-continuation-reifier* values))

;;; (LAMBDA (IGNORE) ...)

(define (effect-continuation-reifier* receiver)
  (lambda (generator)
    (receiver
     (let ((variable (generate-variable 'IGNORE)))
       (make-continuation-abstraction variable (generator))))))

(define effect-continuation-reifier
  (effect-continuation-reifier* values))

;;; (LAMBDA (T) (IF (TRUE? T) ...))

(define (conditional-continuation-reifier* receiver)
  (lambda (generator)
    (receiver
     (let ((variable (generate-variable 'T)))
       (make-continuation-abstraction
        (list variable)
        (generator (make-truth-test (make-reference variable))))))))

(define conditional-continuation-reifier
  (conditional-continuation-reifier* values))

;;;; Frivolous Output Format

;;; We assume for this output format that the input program was
;;; alphatized and has no variables named LAMBDA, LETREC, or IF.  Only
;;; those procedures used in the pages above must be implemented.
;;; Others, such as MAKE-LET, are for the sake of convenience only.

(define (make-literal datum)
  `',datum)

(define (make-reference variable)
  variable)

(define (make-user-abstraction bvl body-constructor)
  (let ((continuation (generate-variable 'C)))
    `(case-lambda ((,continuation . ,bvl)
       ,(body-constructor continuation)))))
       
(define (make-user-abstractions bvls body-constructors)
  (let ((continuation (generate-variable 'C)))
    `(case-lambda
      ,@(map 
        (lambda (bvl body-constructor)
          `((,continuation . ,bvl)
              ,(body-constructor continuation)))
         bvls
         body-constructors))))       

(define (make-continuation-abstraction bvl body)
  `(case-lambda ( ,bvl ,body )))

(define (make-recursive-bind variables constructors body)
  ;; (let ((c (generate-variable 'C)))
  ;;   `(Y (LAMBDA (,c ,@variables)
  ;;         (,c (LAMBDA () ,body)
  ;;             ,@(map (lambda (constructor)
  ;;                      (make-user-abstraction '() constructor))
  ;;                    constructors)))))
  `(letrec ,(map (lambda (variable constructor)
                   `(,variable (,(make-user-abstraction '() constructor) 
                      (letrec-identity ',variable))))
                 variables
                 constructors)
     ,body))
     
(define (make-recursive*-bind variables constructors body)
  `(letrec* ,(map (lambda (variable constructor)
                   `(,variable (,(make-user-abstraction '() constructor) 
                      (letrec*-identity ',variable))))
                 variables
                 constructors)
     ,body))     

(define (make-recursive*-library-bind name variables variables* constructors body)
  `(library-letrec* ,name 
                 ,(map (lambda (variable variable* constructor)
                         `(,variable ,variable* (,(make-user-abstraction '() constructor) 
                            (library-letrec*-identity ',variable))))
                   variables
                   variables*
                   constructors)
     ,body))     
     
(define (make-combination operator continuation operands)
  `(,operator ,continuation ,@operands))

(define (make-return continuation operands)
  `(,continuation ,@operands))

(define (make-conditional condition consequent alternative)
  `(if ,condition ,consequent ,alternative))

(define (make-conditional-join continuation constructor)
  (let ((variable (generate-variable 'J)))
    (make-let (list variable)
              (list continuation)
              (constructor variable))))

(define (make-let variables expressions body)
  `((case-lambda (,variables ,body)) ,@expressions))
  
(define (make-assignment location operand expression)
  `(begin (set! ,location ,operand) ,expression))

(define (make-truth-test operand)
  operand)

(define (generate-variable name)
  (gensym name))
  ;name)


(define (parse x)
  (cond
    [(pair? x) 
     (case (car x)
       [(quote) 
        (cps/generate-literal (cadr x))]
       [(if)
        (cps/generate-conditional (parse (cadr x))
                                  (parse (caddr x))
                                  (parse (cadddr x)))]
       [(case-lambda) 
        (let ((d (cdr x)))
          (cps/generate-abstractions 
            (map car d) 
            (map 
              (lambda (e)
                (cps/generate-sequence (map parse (cdr e))))
              d)))]

       [(letrec) 
        (let ((bindings (cadr x)) (body* (cddr x)))
          (let ((lhs* (map car bindings)) (rhs* (map cadr bindings)))
            (cps/generate-recursive-bind 
              lhs* 
              (map parse rhs*)
              (parse (cons 'begin body*)))))]
       [(letrec*) 
        (let ((bindings (cadr x)) (body* (cddr x)))
          (let ((lhs* (map car bindings)) (rhs* (map cadr bindings)))
          (cps/generate-recursive*-bind 
              lhs* 
              (map parse rhs*)
              (parse (cons 'begin body*)))))]
       [(library-letrec*) 
        (let ((name (cadr x))(x (cdr x)))
          (let ((bindings (cadr x)) (body* (cddr x)))
            (let ((lhs* (map car bindings)) 
                  (lhs** (map cadr bindings)) 
                  (rhs* (map caddr bindings)))
              (cps/generate-recursive*-library-bind 
                name
                lhs* 
                lhs**
                (map parse rhs*)
                (parse (cons 'begin body*))))))]                    
       [(begin) 
        (cps/generate-sequence (map parse (cdr x)))]
       [(set!) 
        (cps/generate-assignment (cadr x) (parse (caddr x)))]
       [else 
        (if (list? x)
            (cps/generate-combination (parse (car x))
                                        (map parse (cdr x)))
            (error 'rewrite "invalid form ~s ~s" x (list? x)))])]
    [(symbol? x)
      (cps/generate-reference x)]
    [else 
      (cps/generate-literal x)]))


(define (starts-with? str sub)
  (clr-call system.string startswith str sub))
  
(define special 
  '(identity-for-cps 
    letrec-identity 
    library-letrec*-identity 
    letrec*-identity 
    cps-prim
    eval-core
    dynamic-wind
    values 
    apply 
    call-with-values 
    call/cc call-with-current-continuation))    
  
(define (primitive? o)  
  (if (and (symbol? o) (not (memq o special)))
    (let ((b (and (symbol-bound? o) (symbol-value o))))
      (or (clr-is ironscheme.runtime.builtinmethod b)
          (clr-generator? o)))
    #f))

(define (clr-generator? o)
  (and (symbol? o) (starts-with? (symbol->string o) "clr-")))       

(define (fix-primitives e)
  (if (and (pair? e)(list? e))
    (let ((o (car e)))
      (if (eq? o 'quote)
        e
        (cond
          [(eq? o 'library-letrec*)
            (let ((name (cadr e))(e (cdr e)))
              (let ((bindings (cadr e)) (body* (cddr e)))
                (let ((lhs* (map car bindings)) 
                      (lhs** (map cadr bindings)) 
                      (rhs* (map caddr bindings)))
                   `(library-letrec* ,name 
                      ,(map list lhs* lhs** (map fix-primitives rhs*))
                      . ,(map fix-primitives body*)))))]
          [(and (clr-generator? o))
            (if (pair? (cdr e))
              (list (fix-primitives (cadr e)) (cons o (map fix-primitives (cddr e))))
              e)]
          [(and (eq? o 'void) (null? (cdr e))) 
            e]
          [else
            (cons (fix-primitives o) (map fix-primitives (cdr e)))])))
    (if (primitive? e)
      `(cps-prim ,e)
      e)))
      
(define (parse->cps e var)      
  ((parse e) (variable-continuator var)))
  
(define (convert->cps e var)
  (fix-primitives (parse->cps e var)))
  
(define bootfile "ironscheme.boot.pp")
(define bootfile-cps "ironscheme.boot.cps")

(define expand-boot-cps
  (case-lambda
    [()       (expand-boot-cps write)]
    [(write)
      (define (read-file port)
        (let f ((e (read port))(a '()))
          (if (eof-object? e)
            (reverse a)
            (let ((r (convert->cps e 'identity-for-cps)))
              (f (read port) (cons r a))))))
      (when (file-exists? bootfile-cps)
        (delete-file bootfile-cps))
      (call-with-input-file bootfile    
        (lambda (in)
          (call-with-output-file bootfile-cps
            (lambda (out)      
              (for-each 
                (lambda (e)
                  (write e out))
                (read-file in))))))]))
  

)

