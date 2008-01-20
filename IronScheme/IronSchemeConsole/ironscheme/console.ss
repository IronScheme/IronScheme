(library (ironscheme console)
  (export
    clear
    beep
    get-title
    set-title!
    get-buffer-height
    get-buffer-width
    set-buffer-size!
    get-window-height
    get-window-width
    set-window-size!
    reset-color  
    get-foreground-color
    set-foreground-color!
    get-foreground-color
    set-foreground-color!
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

  (define (get-title)
    (clr-static-prop-get console title))  
    
  (define (set-title! text)
    (clr-static-prop-set! console title text))  
    
  (define (get-buffer-width)
    (clr-static-prop-get console bufferwidth))  

  (define (get-buffer-height)
    (clr-static-prop-get console bufferheight))  

  (define (set-buffer-size! w h)
    (clr-static-call console setbuffersize w h))       

  (define (get-window-width)
    (clr-static-prop-get console windowwidth))  

  (define (get-window-height)
    (clr-static-prop-get console windowheight))  

  (define (set-window-size! w h)
    (clr-static-call console setwindowsize w h))   
 
  (define (get-foreground-color)
    (clr-static-prop-get console foregroundcolor))     
    
  (define (set-foreground-color! color)
    (clr-static-prop-set! console foregroundcolor color))     

  (define (get-background-color)
    (clr-static-prop-get console backgroundcolor))     
    
  (define (set-background-color! color)
    (clr-static-prop-set! console backgroundcolor color))     

  (define (reset-color)
    (clr-static-call console resetcolor))     
    
    
    
  (clr-clear-usings)
)