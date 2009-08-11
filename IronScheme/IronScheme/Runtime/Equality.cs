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
using System.ComponentModel;
using Microsoft.Scripting;
using System.Diagnostics;
using IronScheme.Compiler;
using Microsoft.Scripting.Ast;
using System.Reflection;

namespace IronScheme.Runtime
{
  public static partial class BuiltinEmitters
  {
    [InlineEmitter("not")]
    public static Expression Not(Expression[] obj)
    {
      if (obj.Length == 1)
      {
        Expression e = Unwrap(obj[0]);
        if (e is UnaryExpression)
        {
          UnaryExpression ue = (UnaryExpression)e;
          if (ue.NodeType == AstNodeType.Not)
          {
            return ue.Operand;
          }
        }
        //if (e is ConditionalExpression)
        //{
        //  ConditionalExpression ce = (ConditionalExpression)e;
        //  return Ast.Condition(ce.Test, ce.IfFalse, ce.IfTrue);
        //}
        if (e.Type == typeof(bool))
        {
          return Ast.Not(e);
        }
        return Ast.Not(Ast.Call(IsTrue, obj[0]));
      }
      return null;
    }

    static MethodInfo IsTrue = typeof(Builtins).GetMethod("IsTrue");

    static Expression Unwrap(Expression ex)
    {
      while (ex is UnaryExpression && ((UnaryExpression)ex).NodeType == AstNodeType.Convert)
      {
        ex = ((UnaryExpression)ex).Operand;
      }

      return ex;
    }

    [InlineEmitter("eq?")]
    public static Expression Eq(Expression[] obj)
    {
      if (obj.Length == 2)
      {
        return Ast.Equal(obj[0], obj[1]);
      }
      return null;
    }

    delegate R Func<R>();
    delegate R Func<T, R>(T t);

    [InlineEmitter("eqv?")]
    public static Expression Eqv(Expression[] obj)
    {
      if (obj.Length == 2)
      {
        Func<Type, bool> p = t => Unwrap(obj[0]).Type == t || Unwrap(obj[1]).Type == t;
        bool vt = !(Unwrap(obj[0]).Type.IsValueType || Unwrap(obj[1]).Type.IsValueType);

        if (p(typeof(SymbolId))
          || p(typeof(bool))
          || (vt && !p(typeof(object)))
          )
        {
          return Ast.Equal(obj[0], obj[1]);
        }
      }
      return null;
    }
  }

  public partial class Builtins
  {

    [Builtin("eq?")]
    public static object IsEqual(object first, object second)
    {
      // 2 exceptions, symbols and booleans (missed the last one somehow, but it will be ref eq)
      if (first is SymbolId && second is SymbolId)
      {
        return GetBool(Equals(first, second));
      }

      return GetBool(ReferenceEquals(first, second));
    }

    [Builtin("eqv?")]
    public static object IsEqualValue(object first, object second)
    {
      if (first is Encoding && second is Encoding)
      {
        return ((Encoding)first).WebName == ((Encoding)second).WebName;
      }
      return GetBool(Equals(first, second));
    }
  }
}
