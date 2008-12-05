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
using IronScheme.Compiler;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting;
using System.Globalization;

namespace IronScheme.Runtime.R6RS.Arithmetic
{
  static class FlonumsInlineEmitters
  {
    static bool Expect(Expression[] args, int count)
    {
      return args.Length == count &&
        Array.TrueForAll(args, delegate(Expression e) { return e.Type == typeof(object) || e.Type == typeof(double); });
    }

    [InlineEmitter("flonum-width")]
    public static Expression FlonumWidth(params Expression[] args)
    {
      if (Expect(args, 0))
      {
        return Ast.Constant(64);
      }
      return null;
    }

    [InlineEmitter("greatest-flonum")]
    public static Expression GreatestFlonum(params Expression[] args)
    {
      if (Expect(args, 0))
      {
        return Ast.Constant(double.MaxValue);
      }
      return null;
    }

    [InlineEmitter("least-flonum")]
    public static Expression LeastFlonum(params Expression[] args)
    {
      if (Expect(args, 0))
      {
        return Ast.Constant(double.MinValue);
      }
      return null;
    }

 
    static Expression Unwrap(Expression e)
    {
      if (e is UnaryExpression)
      {
        UnaryExpression ue = e as UnaryExpression;
        if (ue.NodeType == AstNodeType.Convert)
        {
          return Unwrap(ue.Operand);
        }
      }
      return e;
    }
  }

  public class Flonums : Builtins
  {

    [Builtin("flonum-width")]
    public static object FlonumWidth()
    {
      return 64;
    }

    [Builtin("greatest-flonum")]
    public static object GreatestFlonum()
    {
      return double.MaxValue;
    }

    [Builtin("least-flonum")]
    public static object LeastFlonum()
    {
      return double.MinValue;
    }

    static void ExpectAtLeast(object[] args, double len)
    {
      if (args.Length < len)
      {
        AssertionViolation(GetCaller(), "Expected at least " + len + " arguments, but got " + args.Length + ".", args);
      }
    }

    [Builtin("real->flonum")]
    public static object RealToFlonum(object n)
    {
      // must be number? fixme
      return Convert.ToDouble(n, CultureInfo.InvariantCulture);
    }

    //TODO: overload
    [Builtin("fl=?")]
    public static object FlEqual(params object[] all)
    {
      ExpectAtLeast(all, 2);
      double r = RequiresNotNull<double>(all[0]);
      for (int i = 0; i < all.Length - 1; i++)
      {
        if (r != RequiresNotNull<double>(all[i + 1]))
        {
          return FALSE;
        }
      }
      return TRUE;
    }

    //TODO: overload
    [Builtin("fl>?")]
    public static object FlGreater(params object[] all)
    {
      ExpectAtLeast(all, 2);
      double r = RequiresNotNull<double>(all[0]);
      for (int i = 0; i < all.Length - 1; i++)
      {
        double r2 = RequiresNotNull<double>(all[i + 1]);

        if (!(r > r2))
        {
          return FALSE;
        }
        r = r2;
      }
      return TRUE;
    }

    //TODO: overload
    [Builtin("fl<?")]
    public static object FlLess(params object[] all)
    {
      ExpectAtLeast(all, 2);
      double r = RequiresNotNull<double>(all[0]);
      for (int i = 0; i < all.Length - 1; i++)
      {
        double r2 = RequiresNotNull<double>(all[i + 1]);

        if (!(r < r2))
        {
          return FALSE;
        }
        r = r2;
      }
      return TRUE;
    }

    //TODO: overload
    [Builtin("fl>=?")]
    public static object FlGreaterOrEqual(params object[] all)
    {
      ExpectAtLeast(all, 2);
      double r = RequiresNotNull<double>(all[0]);
      for (int i = 0; i < all.Length - 1; i++)
      {
        double r2 = RequiresNotNull<double>(all[i + 1]);

        if (!(r >= r2))
        {
          return FALSE;
        }
        r = r2;
      }
      return TRUE;
    }

    //TODO: overload
    [Builtin("fl<=?")]
    public static object FlLessOrEqual(params object[] all)
    {
      ExpectAtLeast(all, 2);
      double r = RequiresNotNull<double>(all[0]);
      for (int i = 0; i < all.Length - 1; i++)
      {
        double r2 = RequiresNotNull<double>(all[i + 1]);

        if (!(r <= r2))
        {
          return FALSE;
        }
        r = r2;
      }
      return TRUE;
    }

    [Builtin("fl+")]
    public static object FlAdd()
    {
      return 0.0;
    }

    [Builtin("fl+")]
    public static object FlAdd(object a)
    {
      double x1 = RequiresNotNull<double>(a);
      return x1;
    }


    [Builtin("fl+")]
    public static object FlAdd(object a, object b)
    {
      double x1 = RequiresNotNull<double>(a);
      double x2 = RequiresNotNull<double>(b);

      return x1 + x2;
    }

    [Builtin("fl+")]
    public static object FlAdd(params object[] all)
    {
      double result = 0.0;
      for (int i = 0; i < all.Length; i++)
      {
        result += RequiresNotNull<double>(all[i]);
      }
      return result;
    }

    [Builtin("fl*")]
    public static object FlMultiply()
    {
      return 1.0;
    }

    [Builtin("fl*")]
    public static object FlMultiply(object a)
    {
      double x1 = RequiresNotNull<double>(a);
      return x1;
    }

    [Builtin("fl*")]
    public static object FlMultiply(object a, object b)
    {
      double x1 = RequiresNotNull<double>(a);
      double x2 = RequiresNotNull<double>(b);

      return x1 * x2;
    }

    [Builtin("fl*")]
    public static object FlMultiply(params object[] all)
    {
      double result = 1.0;
      for (int i = 0; i < all.Length; i++)
      {
        result *= RequiresNotNull<double>(all[i]);
      }
      return result;
    }

    [Builtin("fl-")]
    public static object FlMinus(object a)
    {
      double x1 = RequiresNotNull<double>(a);
      return -x1;
    }

    [Builtin("fl-")]
    public static object FlMinus(object a, object b)
    {
      double x1 = RequiresNotNull<double>(a);
      double x2 = RequiresNotNull<double>(b);

      return x1 - x2;
    }

    [Builtin("fl-")]
    public static object FlMinus(params object[] all)
    {
      ExpectAtLeast(all, 1);

      double result = RequiresNotNull<double>(all[0]);
      for (int i = 1; i < all.Length; i++)
      {
        result -= RequiresNotNull<double>(all[i]);
      }
      return result;
    }

    [Builtin("fl/")]
    public static object FlQuotient(object a)
    {
      double x1 = RequiresNotNull<double>(a);
      return 1.0/x1;
    }

    [Builtin("fl/")]
    public static object FlQuotient(object a, object b)
    {
      double x1 = RequiresNotNull<double>(a);
      double x2 = RequiresNotNull<double>(b);
      return x1 / x2;
    }

    [Builtin("fl/")]
    public static object FlQuotient(params object[] all)
    {
      ExpectAtLeast(all, 1);

      double result = RequiresNotNull<double>(all[0]);
      for (int i = 1; i < all.Length; i++)
      {
        result /= RequiresNotNull<double>(all[i]);
      }
      return result;
    }

    //(flnumerator fl) procedure
    [Builtin("flnumerator")]
    public static object FlNumerator(object a)
    {
      if (IsTrue(IsNan(a)) || IsTrue(IsInfinite(a)))
      {
        return a;
      }
      return Convert.ToDouble((((Fraction)RequiresNotNull<double>(a)).Numerator));
    }

    //(fldenominator fl)
    [Builtin("fldenominator")]
    public static object FlDenominator(object a)
    {
      if (IsTrue(IsNan(a)) || IsTrue(IsInfinite(a)))
      {
        return 1.0;
      }
      return Convert.ToDouble((((Fraction)RequiresNotNull<double>(a)).Denominator));
    }


    //(fixnum->flonum fx)
    [Builtin("fixnum->flonum")]
    public static object FixnumToFlonum(object a)
    {
      return (double)RequiresNotNull<int>(a);
    }
  }
}
