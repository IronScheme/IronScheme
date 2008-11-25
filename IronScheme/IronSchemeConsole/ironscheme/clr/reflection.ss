(library (ironscheme clr reflection)
  (export
    method-params
    param-name
    param-type
    method-static?
    member-declaring-type
    member-name
    type-fullname
    type-valuetype?
    type-enum?
    type-members)
  (import 
    (ironscheme)
    (ironscheme clr))

  (define (method-params meth)
    (vector->list (clr-call System.Reflection.MethodBase GetParameters meth)))
    
  (define (param-name p)
    (clr-prop-get System.Reflection.ParameterInfo Name p))

  (define (param-type p)
    (clr-prop-get System.Reflection.ParameterInfo ParameterType p))
    
  (define (method-static? meth)
    (clr-prop-get System.Reflection.MethodBase IsStatic meth))
    
  (define (member-declaring-type mem)
    (clr-prop-get System.Reflection.MemberInfo DeclaringType mem))
    
  (define (member-name mem)
    (clr-prop-get System.Reflection.MemberInfo Name mem))        
    
  (define (type-fullname type)
    (clr-prop-get System.Type FullName type)) 
    
  (define (type-valuetype? type)
    (clr-prop-get System.Type IsValueType type))  

  (define (type-enum? type)
    (clr-prop-get System.Type IsEnum type)) 

  (define type-members  
    (case-lambda
      [(type name) 
        (type-members type name 'all)]
      [(type name member-types)
        (type-members type name member-types 'default)]
      [(type name member-types binding-flags)
        (vector->list
          (clr-call System.Type GetMember type name member-types binding-flags))]))
  

  
  
)