
(match url
  [(calender year month)
    (string-ci=? "calender" calender)
    (context-item-set! 'year year)
    (context-item-set! 'month month)
    (load-controller/action "calender" "show")]
  [(controller)
    #t
    (context-item-set! 'controller controller)
    (context-item-set! 'action "index")
    (load-controller/action controller "index")]
  [(controller action)
    #t
    (when (zero? (string-length action))
      (set! action "index"))
    (context-item-set! 'controller controller)
    (context-item-set! 'action action)
    (load-controller/action controller action)]
  [(controller action id)
    #t
    (context-item-set! 'id id)
    (context-item-set! 'controller controller)
    (context-item-set! 'action action)
    (load-controller/action controller action)])
    