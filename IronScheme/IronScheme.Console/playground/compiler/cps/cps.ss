#!fold-case
;;; -*- Mode: Scheme -*-

;;;; CPS Generator

;;; Copyright (c) 2008, 2009, Taylor R. Campbell
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

;;; This file implements a general, redex-free continuation-passing
;;; style code generator.  The representations of the input and output
;;; are isolated from the details of the CPS.  Clients should define
;;; the CPS-TARGET/ procedures, and call the CPS/ procedures.  The CPS/
;;; procedures return unary procedures that take `continuators', about
;;; which see below; in order to obtain a CPS output term, clients
;;; should usually pass to such a procedure a variable continuator,
;;; constructed by (VARIABLE-CONTINUATOR <variable>) for some
;;; <variable> fit for CPS-TARGET/MAKE-REFERENCE.  For example, to
;;; generate code for (+ 3 4) given a continuation K, assuming that
;;; variables are represented by symbols, one might evaluate
;;;
;;;   ((cps/generate-combination (cps/generate-reference '+ #f)
;;;                              (list (cps/generate-literal 3 #f)
;;;                                    (cps/generate-literal 3 #f))
;;;                              #f)
;;;    (variable-continuator 'K))
;;;
;;; Observe that the use of the abstraction reflects the tree structure
;;; of the input.  The #F arguments supply context that is transmitted
;;; to the CPS-TARGET/ procedures.  For example, they might be replaced
;;; by information about source location.  The transmission of context
;;; to the target is presently a little rudimentary and worth tweaking.

(define (with-cps-generation procedure)
  (with-cps-target procedure))

(define (cps/generate-reference variable context)
  (cps/generate-operand (cps-target/make-reference variable context) context))

(define (cps/generate-literal datum context)
  (cps/generate-operand (cps-target/make-literal datum context) context))

(define (cps/generate-abstraction bvl body-generator context)
  (cps/generate-operand
   (cps-target/make-user-abstraction
    bvl
    (lambda (continuation-variable)
      (body-generator (variable-continuator continuation-variable)))
    context)
   context))

(define (cps/generate-operand operand context)
  (lambda (continuator)
    (continue-with-operand continuator operand context)))

(define (cps/generate-conditional condition-generator
                                  consequent-generator
                                  alternative-generator
                                  context)
  (lambda (continuator)
    (define (generate continuation-variable)
      (let ((continuator* (variable-continuator continuation-variable)))
        (condition-generator
         (conditional-continuator
          (lambda (condition)
            (cps-target/make-conditional condition
                                         (consequent-generator continuator*)
                                         (alternative-generator continuator*)
                                         context))))))
    (reify-continuation continuator context
      generate
      (lambda (continuation-abstraction)
        (cps-target/make-conditional-join continuation-abstraction
                                          generate
                                          context)))))

(define (cps/generate-sequence generators context)
  (lambda (continuator)
    (let recur ((generators generators))
      ((car generators)
       (let ((generators (cdr generators)))
         (if (pair? generators)
             (effect-continuator (lambda () (recur generators)))
             continuator))))))

;;;; Combinations and Tests

(define (cps/generate-combination operator-generator operand-generators
                                  context)
  (cps/generate-subproblems
      (cons operator-generator operand-generators)
      context
    (lambda (operator.operands continuator)
      (let ((make-combination
             (lambda (continuation)
               (cps-target/make-combination (car operator.operands)
                                            continuation
                                            (cdr operator.operands)
                                            context))))
        (reify-continuation continuator context
          make-combination
          make-combination)))))

(define (cps/generate-leaf-combination operator operand-generators context)
  (cps/generate-subproblems operand-generators context
    (lambda (operands continuator)
      (continue-with-operand
       continuator
       (cps-target/make-leaf-combination operator operands context)
       context))))

(define (cps/generate-test predicate operand-generators context)
  (cps/generate-subproblems operand-generators context
    (lambda (operands continuator)
      (continue-with-test continuator
                          (cps-target/make-test predicate operands context)
                          context))))

(define (cps/generate-let-values bvls generators body-generator context)
  (cps/generate-values-binding bvls generators context
    (lambda (subproblems continuator)
      (finish-fork (reverse subproblems) context continuator body-generator))))

(define (cps/generate-letrec-values bvls generators body-generator context)
  (cps/generate-values-binding bvls generators context
    (lambda (subproblems continuator)
      (finish-Y subproblems context continuator body-generator))))

;;; 

;; (define (cps/generate-subproblems-in-order generators context combiner)
;;   (lambda (continuator)
;;     (let loop ((generators generators) (operands '()))
;;       (if (pair? generators)
;;           ((car generators)
;;            (operand-continuator
;;             (let ((generators (cdr generators)))
;;               (lambda (operand)
;;                 (loop generators (cons operand operands))))))
;;           (combiner (reverse operands) continuator)))))

;;;; Subproblems

(define-record-type <subproblem>
    (make-subproblem variables bvl generator context)
    subproblem?
  (variables subproblem.variables)
  (bvl subproblem.bvl)
  (generator subproblem.generator)
  (context subproblem.context))

(define (cps/generate-subproblems generators context combiner)
  (lambda (continuator)
    (let loop ((generators generators) (subproblems '()) (operands '()))
      (if (pair? generators)
          ((car generators)
           (let ((generators (cdr generators)))
             (fork-continuator
              (lambda (operand)
                (loop generators subproblems (cons operand operands)))
              (lambda (variable bvl generator context)
                (loop generators
                      (cons
                       (make-subproblem (list variable) bvl generator context)
                       subproblems)
                      (cons (cps-target/make-reference variable context)
                            operands))))))
          (finish-fork subproblems context continuator
            (lambda (continuator)
              (combiner (reverse operands) continuator)))))))

(define (cps/generate-values-binding bvls generators context finish)
  (lambda (continuator)
    (let loop ((bvls bvls) (generators generators) (subproblems '()))
      (if (pair? bvls)
          (loop (cdr bvls)
                (cdr generators)
                (cons (make-subproblem
                       (cps-target/subproblem-bvl-variables (car bvls) context)
                       (car bvls)
                       (car generators)
                       context)
                      subproblems))
          (finish (reverse subproblems) continuator)))))

(define (finish-fork subproblems context continuator body-generator)
  (reify-continuator continuator context
    (lambda (continuator)
      (let ((body (body-generator continuator)))
        (if (pair? subproblems)
            (if (pair? (cdr subproblems))
                (finish-nontrivial-fork (reverse subproblems) body context)
                (finish-trivial-fork (car subproblems) body context))
            body)))))

(define (finish-nontrivial-fork subproblems body context)
  (finish-subproblems cps-target/make-fork subproblems body context))

(define (finish-trivial-fork subproblem body context)
  ((subproblem.generator subproblem)
   (inline-continuator
    (cps-target/make-subproblem-continuation-abstraction
     (subproblem.variables subproblem)
     (subproblem.bvl subproblem)
     body
     context))))

(define (finish-Y subproblems context continuator body-generator)
  (reify-continuator continuator context
    (lambda (continuator)
      (let ((body (body-generator continuator)))
        (finish-subproblems cps-target/make-Y subproblems body context)))))

(define (finish-subproblems constructor subproblems body context)
  (constructor
   (map (lambda (subproblem)
          (cps-target/make-subproblem
           (subproblem.variables subproblem)
           (subproblem.bvl subproblem)
           (lambda (continuation-variable)
             ((subproblem.generator subproblem)
              (variable-continuator continuation-variable)))
           (subproblem.context subproblem)))
        subproblems)
   body
   context))

;;;; Continuators

;;; A continuator is a meta-description of where the object-program
;;; will proceed next.  We don't always want to generate a full-blown
;;; lambda node for each continuation in the object-program; for
;;; example, if we are generating code for a simple operand, we can
;;; just substitute it directly into wherever the object-program will
;;; continue, such as in an argument position.

;;; In an operand continuator, GENERATOR expects a simple operand.

(define (operand-continuator generator)
  (lambda (if-operand if-effect if-conditional if-variable if-inline if-fork)
    if-effect if-conditional if-variable if-inline if-fork ;ignore
    (if-operand generator)))

;;; In an effect continuator, GENERATOR takes zero arguments; we care
;;; only about effects, not values.

(define (effect-continuator generator)
  (lambda (if-operand if-effect if-conditional if-variable if-inline if-fork)
    if-operand if-conditional if-variable if-inline if-fork ;ignore
    (if-effect generator)))

;;; A conditional continuator is like an operand continuator except
;;; that `conditions' may be somewhat different from operands.  For
;;; instance, the client may require conditions to be combinations, and
;;; change (IF X ...) into (IF (TRUE? X) ...).

(define (conditional-continuator generator)
  (lambda (if-operand if-effect if-conditional if-variable if-inline if-fork)
    if-operand if-effect if-variable if-inline if-fork ;ignore
    (if-conditional generator)))

;;; If we have only a name for the object-continuation, or an
;;; expression, then we use variable or inline continuators.

(define (variable-continuator variable)
  (lambda (if-operand if-effect if-conditional if-variable if-inline if-fork)
    if-operand if-effect if-variable if-inline if-fork ;ignore
    (if-variable variable)))

(define (inline-continuator continuation)
  (lambda (if-operand if-effect if-conditional if-variable if-inline if-fork)
    if-operand if-effect if-conditional if-variable if-fork ;ignore
    (if-inline continuation)))

(define (reified-continuator continuator context)
  (lambda (if-operand if-effect if-conditional if-variable if-inline if-fork)
    if-operand if-effect if-conditional if-fork ;ignore
    (reify-continuation continuator context if-variable if-inline)))

(define (fork-continuator operand-generator subproblem-generator)
  (lambda (if-operand if-effect if-conditional if-variable if-inline if-fork)
    if-operand if-effect if-conditional if-variable if-inline ;ignore
    (if-fork operand-generator
             (lambda (context generator)
               (let ((bvl (cps-target/make-unary-subproblem-bvl context)))
                 (subproblem-generator
                  ;++ Yeck!
                  (car (cps-target/subproblem-bvl-variables bvl context))
                  bvl
                  generator
                  context))))))

;;; The simplest way to use a continuator is when we have a single
;;; operand whose value we want to supply: a literal datum, a variable
;;; reference, a lambda abstraction, or a simple combination.

(define (continue-with-operand continuator operand context)
  (continuator (lambda (generator) (generator operand))
               (lambda (generator) (generator))
               (lambda (generator)
                 (generator (cps-target/make-truth-test operand context)))
               (lambda (continuation-variable)
                 (cps-target/make-return
                  (cps-target/make-reference continuation-variable context)
                  (list operand)
                  context))
               (lambda (continuation-abstraction)
                 (cps-target/make-return continuation-abstraction
                                         (list operand)
                                         context))
               (lambda (operand-generator reified-subproblem-generator)
                 reified-subproblem-generator ;ignore
                 (operand-generator operand))))

(define (continue-with-test continuator test context)
  (continuator (lambda (generator)
                 (reify-test/generate test generator context))
               (lambda (generator) (generator))
               (lambda (generator) (generator test))
               (lambda (continuation-variable)
                 (reify-test test continuation-variable context))
               (lambda (continuation-abstraction)
                 (reify-test/join test continuation-abstraction context))
               (lambda (operand-generator reified-subproblem-generator)
                 operand-generator      ;ignore
                 (reified-subproblem-generator
                  context
                  (lambda (continuator)
                    (continue-with-test continuator test context))))))

(define (reify-test/generate test generator context)
  (reify-test/join test
                   (cps-target/make-value-continuation-abstraction
                    (lambda (variable)
                      (generator
                       (cps-target/make-reference variable context)))
                    context)
                   context))

(define (reify-test/join test continuation context)
  (cps-target/make-conditional-join
   continuation
   (lambda (continuation-variable)
     (reify-test test continuation-variable context))
   context))

(define (reify-test test continuation-variable context)
  (define (branch value)
    (cps-target/make-return
     (cps-target/make-reference continuation-variable context)
     (list (cps-target/make-literal value context))
     context))
  (cps-target/make-conditional test (branch #t) (branch #f) context))

;;;;; Reifying Continuations

(define (reify-continuator continuator context receiver)
  (reify-continuation continuator context
    (lambda (continuation) (receiver (inline-continuator continuation)))
    (lambda (continuation) (receiver (variable-continuator continuation)))))

(define (reify-continuation continuator context if-variable if-inline)
  (continuator
   (operand-continuation-reifier context if-inline)
   (effect-continuation-reifier context if-inline)
   (conditional-continuation-reifier context if-inline)
   if-variable
   if-inline
   (subproblem-continuation-reifier context if-variable if-inline)))

;;; (LAMBDA (VALUE) ...)

(define (operand-continuation-reifier context receiver)
  (lambda (generator)
    (receiver
     (cps-target/make-value-continuation-abstraction
      (lambda (variable)
        (generator (cps-target/make-reference variable context)))
      context))))

;;; (LAMBDA IGNORE ...)

(define (effect-continuation-reifier context receiver)
  (lambda (generator)
    (receiver
     (cps-target/make-ignoring-continuation-abstraction (generator) context))))

;;; (LAMBDA (BOOLEAN) (IF (TRUE? BOOLEAN) ...))

(define (conditional-continuation-reifier context receiver)
  (lambda (generator)
    (receiver
     (cps-target/make-value-continuation-abstraction
      (lambda (variable)
        (generator
         (cps-target/make-truth-test
          (cps-target/make-reference variable context)
          context)))
      context))))

(define (subproblem-continuation-reifier context if-variable if-inline)
  (lambda (operand-generator reified-subproblem-generator)
    operand-generator                   ;ignore
    (reified-subproblem-generator
     context
     (lambda (continuator)
       (reify-continuation continuator context if-variable if-inline)))))
#!no-fold-case