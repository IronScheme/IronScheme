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

(library (ironscheme clr reflection)
  (export
    method?
    param?
    member?
    constructor?
    method-params
    param-name
    param-type
    method-static?
    member-declaring-type
    member-name
    type-generic?
    type-fullname
    type-valuetype?
    type-array?
    type-enum?
    type-member
    type-members)
  (import 
    (ironscheme)
    (ironscheme contracts)
    (ironscheme clr))
    
  (define (method? obj)
    (clr-is System.Reflection.MethodBase obj))  
    
  (define (param? obj)
    (clr-is System.Reflection.ParameterInfo obj))  
     
  (define (member? obj)
    (clr-is System.Reflection.MemberInfo obj))  
    
  (define (constructor? obj)
    (clr-is System.Reflection.ConstructorInfo obj))     
    
  (define (symbol/symbol-list? obj)
    (or 
      (symbol? obj)
      (and (list? obj) (for-all symbol? obj))))      

  (define/contract (method-params meth:method)
    (vector->list (clr-call System.Reflection.MethodBase GetParameters meth)))
    
  (define/contract (param-name p:param)
    (clr-prop-get System.Reflection.ParameterInfo Name p))

  (define/contract (param-type p:param)
    (clr-prop-get System.Reflection.ParameterInfo ParameterType p))
    
  (define/contract (method-static? meth:method)
    (clr-prop-get System.Reflection.MethodBase IsStatic meth))
    
  (define/contract (member-declaring-type mem:member)
    (clr-prop-get System.Reflection.MemberInfo DeclaringType mem))
    
  (define/contract (member-name mem:member)
    (clr-prop-get System.Reflection.MemberInfo Name mem))        
    
  (define/contract (type-fullname type:clr-type)
    (clr-prop-get System.Type FullName type)) 
    
  (define/contract (type-valuetype? type:clr-type)
    (clr-prop-get System.Type IsValueType type))  
    
  (define/contract (type-generic? type:clr-type)
    (clr-prop-get System.Type IsGenericType type))     
    
  (define/contract (type-array? type:clr-type)
    (clr-prop-get System.Type IsArray type))     

  (define/contract (type-enum? type:clr-type)
    (clr-prop-get System.Type IsEnum type)) 
    
  (define/contract type-member
    (case-lambda
      [(type name) 
        (type-member type name 'all)]
      [(type name member-types)
        (type-member type name member-types '(public static instance))]
      [(type:clr-type name:string member-types:symbol/symbol-list binding-flags:symbol/symbol-list)
        (vector->list
          (clr-call System.Type GetMember type name member-types binding-flags))]))
  
  (define/contract type-members
    (case-lambda
      [(type) 
        (type-members type '(public static instance))]
      [(type:clr-type binding-flags:symbol/symbol-list)
         (vector->list
          (clr-call System.Type GetMembers type binding-flags))]))
          
)          

