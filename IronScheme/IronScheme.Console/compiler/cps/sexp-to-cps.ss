#!fold-case
;;; -*- Mode: Scheme -*-

;;;; CPS Generator
;;;; Example Use

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

;;; This uses Alex Shinn's MATCH macro, defined at
;;;
;;; <http://synthcode.com/scheme/match.scm>.

;;; Input language:
;;;
;;; <variable> ::= <symbol>
;;; <expression> ::=
;;; | <variable>                                        Variable references
;;; | (QUOTE <datum>)                                   Literal data
;;;     <datum> ::= any Scheme datum
;;; | (LAMBDA <bvl> <body>)                             Abstractions
;;;     <bvl> ::= <variable> | () | (<variable> . <bvl>)
;;;     <body> ::= <expression>
;;; | (IF <condition> <consequent> <alternative>)       Conditionals
;;;     <condition>, <consequent>, alternative> ::= <expression>
;;; | (LETREC-VALUES (<binding>*) <body>)               Recursive bindings
;;; | (LET-VALUES (<binding>*) <body>)                  Non-recursive bindings
;;;     <binding> ::= (<variable> <expression>)
;;;     <body> ::= <expression>
;;; | (BEGIN <command>* <expression>)                   Command sequences
;;;     <command> ::= <expression>
;;; | (<operator> <operand>*)                           Combination
;;;     <operator>, <operand> ::= <expression>

(define (sexp->cps sexp)
  (let ((context #f))
    (match sexp
      ((? symbol?)
       (cps/generate-reference sexp context))
      (`',datum
       (cps/generate-literal datum context))
      (`(LAMBDA ,bvl ,body)
       (cps/generate-abstraction bvl (sexp->cps body) context))
      (`(IF ,condition ,consequent ,alternative)
       (cps/generate-conditional (sexp->cps condition)
                                 (sexp->cps consequent)
                                 (sexp->cps alternative)
                                 context))
      (`(LETREC-VALUES ,bindings ,body)
       (cps/generate-letrec-values (map car bindings)
                                   (map sexp->cps (map cadr bindings))
                                   (sexp->cps body)
                                   context))
      (`(LET-VALUES ,bindings ,body)
       (cps/generate-let-values (map car bindings)
                                (map sexp->cps (map cadr bindings))
                                (sexp->cps body)
                                context))
      (`(BEGIN ,command . ,commands)
       (cps/generate-sequence (map sexp->cps (cons command commands)) context))
      (`(,operator . ,operands)
       (cps/generate-combination (sexp->cps operator)
                                 (map sexp->cps operands)
                                 context))
      (else
       (error "Invalid form:" sexp)))))
#!no-fold-case