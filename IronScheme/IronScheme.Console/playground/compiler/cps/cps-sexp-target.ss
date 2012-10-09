#!fold-case
;;; -*- Mode: Scheme -*-

;;;; CPS Generator
;;;; Example Target

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

;;; Informal description of the output language:
;;;
;;; <operand> ::= <variable> | <literal> | <abstraction> | <leaf-combination>
;;;   <variable> ::= <symbol>
;;;   <literal> ::= (QUOTE <datum>)
;;;     <datum> ::= any Scheme datum
;;;   <abstraction> ::= (LAMBDA <bvl> <body>)
;;;     <bvl> ::= <symbol> | () | (<symbol> . <bvl>)
;;;   <leaf-combination> ::= (<operator> <operand>*)
;;;     <operator> ::= <operand>
;;;
;;; <body> ::= <letrec-values> | <let-values> | <conditional> | <combination>
;;;   <letrec-values> ::= (LETREC-VALUES (<binding>*) <body>)
;;;   <let-values> ::= (LET-VALUES (<binding>*) <body>)
;;;     <binding> ::= (<variable> <abstraction>)
;;;         Note: The <abstraction> is a procedure called with a
;;;         continuation to which it must pass the variable's value;
;;;         LET-VALUES and LETREC-VALUES are not limited to lambdas
;;;         on the right-hand sides.
;;;   <conditional> ::= (IF <condition> <consequent> <alternative>)
;;;     <consequent>, <alternative> ::= <body>
;;;   <combination> ::= (<operator> <operand>*)
;;;     <operator> ::= <operand>
;;;
;;; <condition> ::= (<predicate> <operand>*)
;;;   <predicate> ::= TRUE?, ...

(define (with-cps-target procedure)
  (with-variable-numbering procedure))

(define (cps-target/make-reference variable context)
  context                               ;ignore
  variable)

(define (cps-target/make-literal datum context)
  context                               ;ignore
  `',datum)

(define (cps-target/make-user-abstraction bvl body-constructor context)
  context                               ;ignore
  (let ((continuation-variable (generate-variable 'C)))
    `(LAMBDA (,continuation-variable . ,bvl)
       ,(body-constructor continuation-variable))))

(define (cps-target/make-conditional condition consequent alternative context)
  context                               ;ignore
  `(IF ,condition ,consequent ,alternative))

(define (cps-target/make-conditional-join abstraction constructor context)
  context                               ;ignore
  (let ((join-variable (generate-variable 'J)))
    `((LAMBDA (,join-variable)
        ,(constructor join-variable))
      ,abstraction)))

(define (cps-target/make-combination operator continuation operands context)
  context                               ;ignore
  `(,operator ,continuation ,@operands))

(define (cps-target/make-leaf-combination operator operands context)
  context                               ;ignore
  `(,operator ,@operands))

(define (cps-target/make-test predicate operands context)
  context                               ;ignore
  `(,predicate ,@operands))

(define-record-type <target-subproblem>
    (make-target-subproblem variables bvl expression)
    target-subproblem?
  (variables target-subproblem.variables)
  (bvl target-subproblem.bvl)
  (expression target-subproblem.expression))

(define (cps-target/make-subproblem variables bvl constructor context)
  context                               ;ignore
  (make-target-subproblem variables
                          bvl
                          (let ((continuation-variable (generate-variable 'C)))
                            `(LAMBDA (,continuation-variable)
                               ,(constructor continuation-variable)))))

(define (cps-target/make-fork subproblems join context)
  context                               ;ignore
  ;; `(FORK (LAMBDA ,(append-map target-subproblem.variables subproblems)
  ;;          ,join)
  ;;        ,@(map target-subproblem.expression subproblems))
  `(LET-VALUES ,(map (lambda (subproblem)
                       `(,(target-subproblem.bvl subproblem)
                         ,(target-subproblem.expression subproblem)))
                     subproblems)
     ,join))

(define (cps-target/make-Y subproblems body context)
  context                               ;ignore
  ;; `(Y (LAMBDA ,@(append-map target-subproblem.variables subproblems)
  ;;       (Y* (LAMBDA () ,body)
  ;;           ,@(map target-subproblem.expression subproblems))))
  `(LETREC-VALUES ,(map (lambda (subproblem)
                          `(,(target-subproblem.bvl subproblem)
                            ,(target-subproblem.expression subproblem)))
                        subproblems)
     ,body))

(define (cps-target/make-unary-subproblem-bvl context)
  (list (generate-variable 'VALUE)))

(define (cps-target/subproblem-bvl-variables bvl context)
  context                               ;ignore
  (let recur ((bvl bvl))
    (cond ((pair? bvl) (cons (car bvl) (recur (cdr bvl))))
          ((symbol? bvl) (list (car bvl)))
          ((null? bvl) '())
          (else (error "Bad BVL:" bvl)))))

(define (cps-target/make-truth-test operand context)
  context                               ;ignore
  `(TRUE? ,operand))

(define (cps-target/make-return continuation operands context)
  context                               ;ignore
  `(,continuation ,@operands))

(define (cps-target/make-subproblem-continuation-abstraction variables
                                                             bvl
                                                             body
                                                             context)
  variables context                     ;ignore
  `(LAMBDA ,bvl ,body))

(define (cps-target/make-value-continuation-abstraction body-constructor
                                                        context)
  context                               ;ignore
  (let ((variable (generate-variable 'VALUE)))
    `(LAMBDA (,variable)
       ,(body-constructor variable))))

(define (cps-target/make-ignoring-continuation-abstraction body context)
  context                               ;ignore
  `(LAMBDA ,(generate-variable 'IGNORE) ,body))

(define *variable-number* 0)

(define (with-variable-numbering procedure)
  (dynamic-bind (lambda () *variable-number*)
                (lambda (number) (set! *variable-number* number))
                0
                procedure))

(define (generate-variable name)
  (string->symbol
   (string-append
    (symbol->string name)
    "."
    (number->string
     (let ((number *variable-number*))
       (set! *variable-number* (+ number 1))
       number)))))

(define (dynamic-bind fetch store value procedure)
  (define (swap)
    (let ((temporary (fetch)))
      (store value)
      (set! value temporary)))
  (dynamic-wind swap procedure swap))
#!no-fold-case