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

(library (ironscheme console)
  (export
    clear
    beep
    window-title
    buffer-height
    buffer-width
    set-buffer-size!
    window-height
    window-width
    set-window-size!
    reset-color  
    foreground-color
    background-color
    )
  (import 
    (rnrs)
    (ironscheme contracts)
    (ironscheme clr))

  (clr-using System)
  
  (define (clear)
    (clr-static-call Console Clear))   
  
  (define/contract beep
    (case-lambda
      [()
        (clr-static-call Console Beep)]
      [(freq:fixnum dur:fixnum)   
        (clr-static-call Console Beep freq dur)]))

  (define/contract window-title
    (case-lambda
      [()
        (clr-static-prop-get Console Title)]
      [(text:string)
        (clr-static-prop-set! Console Title text)]))
    
  (define/contract buffer-width
    (case-lambda
      [()
        (clr-static-prop-get Console BufferWidth)]
      [(value:fixnum)      
        (clr-static-prop-set! Console BufferWidth value)]))

  (define/contract buffer-height
    (case-lambda
      [()
        (clr-static-prop-get Console BufferHeight)]
      [(value:fixnum)
        (clr-static-prop-set! Console BufferHeight value)]))

  (define/contract (set-buffer-size! w:fixnum h:fixnum)
    (clr-static-call Console SetBufferSize w h))       

  (define/contract window-width
    (case-lambda
      [()
        (clr-static-prop-get Console WindowWidth)]
      [(value:fixnum)      
        (clr-static-prop-set! Console WindowWidth value)]))

  (define/contract window-height
    (case-lambda
      [()
        (clr-static-prop-get Console WindowHeight)]
      [(value:fixnum)      
        (clr-static-prop-set! Console WindowHeight value)]))

  (define (set-window-size! w h)
    (clr-static-call Console SetWindowSize w h))   
 
  (define/contract foreground-color
    (case-lambda
      [()           
        (clr-static-prop-get Console ForegroundColor)]
      [(color:symbol)      
        (clr-static-prop-set! Console ForegroundColor color)]))

  (define/contract background-color
    (case-lambda
      [()           
        (clr-static-prop-get Console BackgroundColor)]
      [(color:symbol)      
        (clr-static-prop-set! Console BackgroundColor color)]))

  (define (reset-color)
    (clr-static-call Console ResetColor))     

)