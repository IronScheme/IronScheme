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
    (ironscheme clr))

  (clr-using system)
  
  (define (clear)
    (clr-static-call console clear))   
  
  (define beep
    (case-lambda
      [()           (clr-static-call console beep)]
      [(freq dur)   (clr-static-call console beep freq dur)]))

  (define window-title
    (case-lambda
      [()           (clr-static-prop-get console title)]
      [(text)       (clr-static-prop-set! console title text)]))
    
  (define buffer-width
    (case-lambda
      [()           (clr-static-prop-get console bufferwidth)]
      [(value)      (clr-static-prop-get! console bufferwidth value)]))

  (define buffer-height
    (case-lambda
      [()           (clr-static-prop-get console bufferheight)]
      [(value)      (clr-static-prop-get! console bufferheight value)]))

  (define (set-buffer-size! w h)
    (clr-static-call console setbuffersize w h))       

  (define window-width
    (case-lambda
      [()           (clr-static-prop-get console windowwidth)]
      [(value)      (clr-static-prop-get! console windowwidth value)]))

  (define window-height
    (case-lambda
      [()           (clr-static-prop-get console windowheight)]
      [(value)      (clr-static-prop-get! console windowheight value)]))

  (define (set-window-size! w h)
    (clr-static-call console setwindowsize w h))   
 
  (define foreground-color
    (case-lambda
      [()           (clr-static-prop-get console foregroundcolor)]
      [(color)      (clr-static-prop-set! console foregroundcolor color)]))

  (define background-color
    (case-lambda
      [()           (clr-static-prop-get console backgroundcolor)]
      [(color)      (clr-static-prop-set! console backgroundcolor color)]))

  (define (reset-color)
    (clr-static-call console resetcolor))     
    
    
    
  (clr-clear-usings)
)