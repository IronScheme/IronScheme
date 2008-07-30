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
    [InlineEmitter("symbol?")]
    public static Expression IsSymbol(Expression[] obj)
    {
      return Ast.TypeIs(obj[0], typeof(SymbolId));
    }

    [InlineEmitter("boolean?")]
    public static Expression IsBoolean(Expression[] obj)
    {
      return Ast.TypeIs(obj[0], typeof(bool));
    }

    [InlineEmitter("procedure?")]
    public static Expression IsProcedure(Expression[] obj)
    {
      return Ast.TypeIs(obj[0], typeof(ICallable));
    }

    [InlineEmitter("not")]
    public static Expression Not(Expression[] obj)
    {
      Expression e = Unwrap(obj[0]);
      if (e.Type == typeof(bool))
      {
        return Ast.Not(e);
      }
      return Ast.Condition(Ast.TypeIs(obj[0], typeof(bool)), Ast.Not(Ast.Call(IsTrue, obj[0])), Ast.False());
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

  }

  public partial class Builtins
  {
    static bool EqualCons(Cons a, Cons b)
    {
      if (ReferenceEquals(a,b))
      {
        return true;
      }
      if (a == null || b == null)
      {
        return false;
      }

      if (a.IsCyclic || b.IsCyclic)
      {
        return IsTrue(IsEquivalent(a.car, b.car)) && a.cdr == b.cdr;
      }

      return IsTrue(IsEquivalent(a.car, b.car)) && EqualCons(a.cdr as Cons, b.cdr as Cons);
    }

    [Builtin("equal?")]
    public static object IsEquivalent(object first, object second)
    {

      if (first == second)
      {
        return TRUE;
      }

      if (first == null ^ second == null)
      {
        return FALSE;
      }

      bool s1 = first is SymbolId;
      bool s2 = second is SymbolId;

      bool c1 = first is Cons;
      bool c2 = second is Cons;

      if (s1 && c2 || s2 && c1)
      {
        return FALSE;
      }

      if (c1 && c2)
      {
        Cons cc1 = first as Cons;
        Cons cc2 = second as Cons;
        return GetBool(EqualCons(cc1, cc2));
      }

      if (first is byte[] && second is byte[])
      {
        byte[] f = first as byte[];
        byte[] s = second as byte[];

        if (f.Length != s.Length)
        {
          return FALSE;
        }
        else
        {
          for (int i = 0; i < f.Length; i++)
          {
            if (!Equals(f[i], s[i]))
            {
              return FALSE;
            }
          }

          return TRUE;
        }
      }

      if (first is object[] && second is object[])
      {
        object[] f = first as object[];
        object[] s = second as object[];

        if (f.Length != s.Length)
        {
          return FALSE;
        }
        else
        {
          for (int i = 0; i < f.Length; i++)
          {
            if (!IsTrue(IsEquivalent(f[i], s[i])))
            {
              return FALSE;
            }
          }

          return TRUE;
        }
      }

      Type t1 = first.GetType();
      Type t2 = second.GetType();

      if (t1 == t2)
      {
        return GetBool(Equals(first, second));
      }

      if (IsTrue(IsNumber(first)) && IsTrue(IsNumber(second)))
      {
        return IsSame(first, second);
      }

      string w1 = WriteFormat(first);
      string w2 = WriteFormat(second);

      bool result = w1 == w2;

      return GetBool(result);
    }

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
      return GetBool(Equals(first, second));
    }

  }
}
