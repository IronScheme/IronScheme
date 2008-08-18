(library (views calender)
  (export
    show)
  (import
    (ironscheme)
    (ironscheme clr)
    (ironscheme web))
  
  (clr-reference "System.Web, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a")
  
  (define (show year month) 
    (let ((tb (clr-new System.Web.UI.WebControls.Calendar)))
      (clr-prop-set! System.Web.UI.WebControls.Calendar VisibleDate tb (clr-new System.DateTime year month 1))
      (clr-call System.Web.UI.WebControls.Calendar RenderControl tb 
        (clr-new System.Web.UI.HtmlTextWriter (http-output-port)))))
)