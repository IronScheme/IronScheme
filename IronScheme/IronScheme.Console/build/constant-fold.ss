#| ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 2007,2008,2009
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************|#

(library (ironscheme constant-fold)
  (export 
    enable-constant-fold
    enable-constant-fold/env)
  (import (ironscheme)
          (ironscheme clr))
  
  (define (allow-constant-fold? proc)
    (clr-prop-get IronScheme.Runtime.Callable AllowConstantFold proc))

  (define (set-allow-constant-fold! proc bool)
    (clr-prop-set! IronScheme.Runtime.Callable AllowConstantFold proc bool))
    
  (define (enable-constant-fold . procs)
    (for-each (lambda (proc)
                (set-allow-constant-fold! proc #t))
              procs))
              
  (define (proc-filter b)
    (eq? 'procedure (cdr b)))
              
  (define (enable-constant-fold/env . import-spec)
    (let ((env (apply environment import-spec))
          (env* (apply environment '(only (rnrs) list) import-spec)))
      (let ((b (map car (filter proc-filter (environment-bindings env)))))
        (apply enable-constant-fold (eval `(list ,@b) env*)))))
        
)          
  