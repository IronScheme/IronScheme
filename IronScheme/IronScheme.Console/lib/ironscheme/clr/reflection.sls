#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme clr reflection)
  (export
    method?
    property?
    field?
    param?
    member?
    property-get-value
    property-set-value
    property-type
    field-get-value
    field-set-value
    field-type
    constructor?
    method-params
    method-invoke
    method-contains-generic-parameters?
    param-name
    param-type
    method-static?
    member-declaring-type
    member-name
    type-name
    type-namespace
    type-assignable-from?
    type-generic?
    type-fullname
    type-valuetype?
    type-array?
    type-enum?
    find-type-members
    type-member
    type-members)
  (import 
    (ironscheme)
    (ironscheme contracts)
    (ironscheme clr))
    
  (clr-using System.Reflection)    
    
  (define (method? obj)
    (clr-is MethodBase obj))  
    
  (define (param? obj)
    (clr-is ParameterInfo obj))  
     
  (define (member? obj)
    (clr-is MemberInfo obj))  

  (define (property? obj)
    (clr-is PropertyInfo obj))  

  (define (field? obj)
    (clr-is FieldInfo obj))  
    
  (define (constructor? obj)
    (clr-is ConstructorInfo obj))     
    
  (define (symbol/symbol-list? obj)
    (or 
      (symbol? obj)
      (and (list? obj) (for-all symbol? obj)))) 
      
  (define/contract (method-contains-generic-parameters? meth:method)
    (clr-prop-get MethodBase ContainsGenericParameters meth))          

  (define/contract (method-params meth:method)
    (vector->list (clr-call MethodBase GetParameters meth)))
    
  (define/contract (param-name p:param)
    (clr-prop-get ParameterInfo Name p))

  (define/contract (param-type p:param)
    (clr-prop-get ParameterInfo ParameterType p))
    
  (define/contract (method-invoke meth:method instance args)
    (clr-call MethodBase Invoke meth instance (list->vector args)))
    
  (define/contract (property-get-value prop:property instance args)
    (clr-call PropertyInfo GetValue prop instance (list->vector args)))

  (define/contract (field-get-value fld:field instance)
    (clr-call FieldInfo GetValue fld instance))
    
  (define/contract (property-set-value prop:property instance args value)
    (clr-call PropertyInfo SetValue prop instance value (list->vector args)))

  (define/contract (field-set-value fld:field instance value)
    (clr-call FieldInfo SetValue fld instance value))    

  (define/contract (property-type prop:property)
    (clr-prop-get PropertyInfo PropertyType prop))
    
  (define/contract (field-type fld:field)
    (clr-prop-get FieldInfo FieldType fld))
    
  (define/contract (method-static? meth:method)
    (clr-prop-get MethodBase IsStatic meth))
    
  (define/contract (member-declaring-type mem:member)
    (clr-prop-get MemberInfo DeclaringType mem))
    
  (define/contract (member-name mem:member)
    (clr-prop-get MemberInfo Name mem))        
    
  (define/contract (type-assignable-from? type:clr-type fromtype:clr-type)
    (clr-call Type IsAssignableFrom type fromtype))
    
  (define/contract (type-fullname type:clr-type)
    (clr-prop-get Type FullName type)) 
    
  (define/contract (type-namespace type:clr-type)
    (clr-prop-get Type Namespace type)) 
    
  (define/contract (type-name type:clr-type)
    (clr-prop-get Type Name type))         
    
  (define/contract (type-valuetype? type:clr-type)
    (clr-prop-get Type IsValueType type))  
    
  (define/contract (type-generic? type:clr-type)
    (clr-prop-get Type IsGenericType type))     
    
  (define/contract (type-array? type:clr-type)
    (clr-prop-get Type IsArray type))     

  (define/contract (type-enum? type:clr-type)
    (clr-prop-get Type IsEnum type)) 
    
  (define/contract (find-type-members type:clr-type 
                                      member-types:symbol/symbol-list 
                                      binding-flags:symbol/symbol-list
                                      filter:procedure
                                      criteria)
    (vector->list
      (clr-call Type FindMembers type member-types binding-flags filter criteria)))
    
  (define/contract type-member
    (case-lambda
      [(type name) 
        (type-member type name 'all)]
      [(type name member-types)
        (type-member type name member-types '(public static instance))]
      [(type:clr-type name:string member-types:symbol/symbol-list binding-flags:symbol/symbol-list)
        (vector->list
          (clr-call Type GetMember type name member-types binding-flags))]))
  
  (define/contract type-members
    (case-lambda
      [(type) 
        (type-members type '(public static instance))]
      [(type:clr-type binding-flags:symbol/symbol-list)
         (vector->list
          (clr-call Type GetMembers type binding-flags))])))          

