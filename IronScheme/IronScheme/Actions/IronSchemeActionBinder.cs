#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
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
using IronScheme.Runtime;

namespace IronScheme.Actions
{
  class IronSchemeActionBinder : ActionBinder
  {
    public IronSchemeActionBinder(CodeContext cc)
      : base(cc)
    {
      
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
      if (fromType == typeof(SymbolId) && toType.IsEnum)
      {
        return true;
      }
      if (fromType == typeof(object) && level == NarrowingLevel.All)
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
      return true;
    }

    public override Expression ConvertExpression(Expression expr, Type toType)
    {
      if (toType == typeof(object))
      {
        return expr;
      }
      Expression expr2 = IronScheme.Compiler.Generator.Unwrap(expr);
      if (expr2.Type == toType)
      {
        return expr2;
      }
      return Ast.ConvertHelper(expr, toType);
    }

    public override Expression CheckExpression(Expression expr, Type toType)
    {
      throw new Exception("The method or operation is not implemented.");
    }
  }
}
