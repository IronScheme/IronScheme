(import 
  (ironscheme)
  (ironscheme web)
  (prefix (snippets) snippet:))
  
(case (http-method)
  [(get)
    (snippet:load (querystring 'id))]
  [(post)
    (snippet:save (form 'name) (form 'expr))])
      