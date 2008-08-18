(library (controllers calender)
  (export
    show)
  (import
    (ironscheme)
    (ironscheme web controllers)
    (prefix (views calender) view-))
 
  (define-action (show year month)
    (view-show (string->number year) (string->number month)))  
)    