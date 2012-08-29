#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Reflection;
using System.Text;
using IronScheme.Compiler;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;

namespace IronScheme.Runtime
{
  public static partial class BuiltinEmitters
  {
    [InlineEmitter("not")]
    public static Expression Not(params Expression[] obj)
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
        if (e is ConditionalExpression)
        {
          ConditionalExpression ce = (ConditionalExpression)e;
          return Ast.Condition(ce.Test, Not(ce.IfTrue),  Not(ce.IfFalse));
        }
        if (e.Type == typeof(bool))
        {
          return Ast.Not(e);
        }
        //return Ast.Equal(Ast.Constant(false), obj[0]);
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
        else if (Unwrap(obj[0]) is ConstantExpression || Unwrap(obj[1]) is ConstantExpression)
        {
          return Ast.Call(typeof(object).GetMethod("Equals", BindingFlags.Public | BindingFlags.Static), obj); 
        }
      }
      return null;
    }
  }

  public partial class Builtins
  {
    // can't move this, gets inlined most of the time
    [Builtin("eq?")]
    public static object IsEqual(object first, object second)
    {
      return GetBool(ReferenceEquals(first, second));
    }

    // can't move this, gets inlined most of the time
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
