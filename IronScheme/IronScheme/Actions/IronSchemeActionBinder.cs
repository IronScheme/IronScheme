#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Permissive License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Permissive License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting;
using Microsoft.Scripting.Generation;
using System.ComponentModel;
using IronScheme.Compiler;
using System.Reflection;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Types;

namespace IronScheme.Actions
{
  class IronSchemeActionBinder : ActionBinder
  {
    public IronSchemeActionBinder(CodeContext cc)
      : base(cc)
    {
      
    }

    protected override StandardRule<T> MakeRule<T>(CodeContext callerContext, DynamicAction action, object[] args)
    {
      //if (args.Length > 1 && args[0] is SymbolId)
      //{
      //  SymbolId membername = (SymbolId)args[0];
      //  object target = args[1];
      //  Type targettype = null;
      //  BindingFlags sta = BindingFlags.Static;
      //  if (target is SymbolId)
      //  {
      //    targettype = TypeResolver.GetType(SymbolTable.IdToString((SymbolId)target));
      //  }
      //  else
      //  {
      //    sta = BindingFlags.Instance;
      //    targettype = target.GetType();
      //  }
      //  MethodBase targetmem = targettype.GetMethod(SymbolTable.IdToString(membername), BindingFlags.Public | sta | BindingFlags.IgnoreCase);

      //  BuiltinFunction bif = BuiltinFunction.MakeMethod(SymbolTable.IdToString(membername), targetmem, sta == BindingFlags.Instance ? FunctionType.Method : FunctionType.Function);

      //  args[0] = bif;

      //  return base.MakeRule<T>(callerContext, action, args);
      //}
      //else
      {
        return base.MakeRule<T>(callerContext, action, args);
      }
    }

    public override bool CanConvertFrom(Type fromType, Type toType, NarrowingLevel level)
    {
      if (fromType == toType || toType.IsAssignableFrom(fromType))
      {
        return true;
      }
      if (toType == typeof(object) || toType == fromType)
      {
        return true;
      }
      if (fromType == typeof(int) && toType == typeof(double))
      {
        return true;
      }
      return false;
    }

    public override object Convert(object obj, Type toType)
    {
      if (obj == null)
      {
        if (!toType.IsValueType)
        {
          return null;
        }
      }
      if (obj != null && toType.IsAssignableFrom(obj.GetType()))
      {
        return obj;
      }
      if (obj != null && obj.GetType().IsSubclassOf(toType))
      {
        return obj;
      }
      TypeConverter tc = TypeDescriptor.GetConverter(obj);
      return tc.ConvertTo(obj, toType);
    }

    public override void EmitConvertFromObject(CodeGen cg, Type paramType)
    {
      if (paramType == typeof(void))
      {
        cg.EmitFieldGet(Compiler.Generator.Unspecified);
      }
      else
      {
        cg.EmitCast(typeof(object), paramType);
      }
    }

    public override bool PreferConvert(Type t1, Type t2)
    {
      throw new Exception("The method or operation is not implemented.");
    }

    public override Expression ConvertExpression(Expression expr, Type toType)
    {
      if (toType == typeof(object))
      {
        return expr;
      }
      if (expr.Type == toType)
      {
        return expr;
      }
      return Ast.DynamicConvert(expr, toType);
      //throw new NotImplementedException("The method or operation is not implemented.");
    }

    public override Expression CheckExpression(Expression expr, Type toType)
    {
      throw new Exception("The method or operation is not implemented.");
    }
  }
}
