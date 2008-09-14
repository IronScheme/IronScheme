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

namespace IronScheme.Runtime.R6RS.Arithmetic
{
  static class FlonumsInlineEmitters
  {
    static bool Expect(Expression[] args, int count)
    {
      return args.Length == count &&
        Array.TrueForAll(args, delegate(Expression e) { return e.Type == typeof(object) || e.Type == typeof(double); });
    }

    [InlineEmitter("flonum?")]
    public static Expression IsFlonum(params Expression[] args)
    {
      if (Expect(args, 1))
      {
        return Ast.TypeIs(Unwrap(args[0]), typeof(double));
      }
      return null;
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
    [Builtin("flonum?")]
    public static object IsFlonum(object o)
    {
      return GetBool(o is double);
    }

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
      return Convert.ToDouble(n);
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

    [Builtin("flinteger?")]
    public static object FlIsInteger(object a)
    {
      double x1 = RequiresNotNull<double>(a);
      return GetBool(x1 % 1.0 == 0.0);
    }

    [Builtin("flzero?")]
    public static object FlIsZero(object a)
    {
      double x1 = RequiresNotNull<double>(a);
      return GetBool(x1 == 0.0);
    }

    [Builtin("flpositive?")]
    public static object FlIsPositive(object a)
    {
      double x1 = RequiresNotNull<double>(a);
      return GetBool(x1 > 0);
    }

    [Builtin("flnegative?")]
    public static object FlIsNegative(object a)
    {
      double x1 = RequiresNotNull<double>(a);
      return GetBool(x1 < 0);
    }

    [Builtin("flodd?")]
    public static object FlIsOdd(object a)
    {
      double x1 = RequiresNotNull<double>(a);
      return GetBool(Math.Abs(x1 % 2) == 1.0);
    }

    [Builtin("fleven?")]
    public static object FlIsEven(object a)
    {
      double x1 = RequiresNotNull<double>(a);
      return GetBool(x1 % 2 == 0.0);
    }

    [Builtin("flfinite?")]
    public static object FlIsFinite(object a)
    {
      double x1 = RequiresNotNull<double>(a);
      return GetBool(!double.IsInfinity(x1));
    }

    [Builtin("flinfinite?")]
    public static object FlIsInfinite(object a)
    {
      double x1 = RequiresNotNull<double>(a);
      return GetBool(double.IsInfinity(x1));
    }

    [Builtin("flnan?")]
    public static object FlIsNan(object a)
    {
      double x1 = RequiresNotNull<double>(a);
      return GetBool(double.IsNaN(x1));
    }


    [Builtin("flmax")]
    public static object FlMax(params object[] args)
    {
      ExpectAtLeast(args, 1);
      int arglen = args.Length;
      switch (arglen)
      {
        case 1:
          return RequiresNotNull<double>(args[0]);
        case 2:
          return Math.Max(RequiresNotNull<double>(args[0]), RequiresNotNull<double>(args[1]));
        default:
          object[] head = ArrayUtils.RemoveLast(args);
          return FlMax(FlMax(head), args[arglen - 1]);
      }
    }

    [Builtin("flmin")]
    public static object FlMin(params object[] args)
    {
      ExpectAtLeast(args, 1);
      int arglen = args.Length;
      switch (arglen)
      {
        case 1:
          return RequiresNotNull<double>(args[0]);
        case 2:
          return Math.Min(RequiresNotNull<double>(args[0]), RequiresNotNull<double>(args[1]));
        default:
          object[] head = ArrayUtils.RemoveLast(args);
          return FlMin(FlMin(head), args[arglen - 1]);
      }
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

    [Builtin("flabs")]
    public static object FlAbs(object a)
    {
      double x1 = RequiresNotNull<double>(a);
      return Math.Abs(x1);
    }

    [Builtin("fldiv-and-mod")]
    public static object FlDivAndMod(object a, object b)
    {
      double x1 = RequiresNotNull<double>(a);
      double x2 = RequiresNotNull<double>(b);

      MultipleValues r = (MultipleValues)DivMod(x1, x2);
      r[0] = Convert.ToDouble(r[0]);

      return r;
    }

    [Builtin("fldiv")]
    public static object FlDiv(object a, object b)
    {
      return ((MultipleValues)FlDivAndMod(a, b))[0];
    }


    [Builtin("flmod")]
    public static object FlMod(object a, object b)
    {
      return ((MultipleValues)FlDivAndMod(a, b))[1];
    }

    [Builtin("fldiv0-and-mod0")]
    public static object FlDiv0AndMod0(object a, object b)
    {
      double x1 = RequiresNotNull<double>(a);
      double x2 = RequiresNotNull<double>(b);

      MultipleValues r = (MultipleValues)Div0Mod0(x1, x2);
      r[0] = Convert.ToDouble(r[0]);

      return r;

    }

    [Builtin("fldiv0")]
    public static object FlDiv0(object a, object b)
    {
      return ((MultipleValues)FlDiv0AndMod0(a, b))[0];
    }


    [Builtin("flmod0")]
    public static object FlMod0(object a, object b)
    {
      return ((MultipleValues)FlDiv0AndMod0(a, b))[1];
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

    //(flfloor fl) procedure
    [Builtin("flfloor")]
    public static object FlFloor(object a)
    {
      return Math.Floor(RequiresNotNull<double>(a));
    }

    //(flceiling fl) procedure
    [Builtin("flceiling")]
    public static object FlCeiling(object a)
    {
      return Math.Ceiling(RequiresNotNull<double>(a));
    }

    //(fltruncate fl) procedure
    [Builtin("fltruncate")]
    public static object FlTruncate(object a)
    {
      return Math.Truncate(RequiresNotNull<double>(a));
    }

    //(flround fl)
    [Builtin("flround")]
    public static object FlRound(object a)
    {
      return Math.Round(RequiresNotNull<double>(a));
    }

    //(flexp fl) procedure
    [Builtin("flexp")]
    public static object FlExp(object a)
    {
      return Math.Exp(RequiresNotNull<double>(a));
    }

    //(fllog fl) procedure
    [Builtin("fllog")]
    public static object FlLog(object a)
    {
      return Math.Log(RequiresNotNull<double>(a));
    }

    //(fllog fl1 fl2) procedure
    [Builtin("fllog")]
    public static object FlLog(object a, object b)
    {
      return Math.Log(RequiresNotNull<double>(a), RequiresNotNull<double>(b));
    }

    //(flsin fl) procedure
    [Builtin("flsin")]
    public static object FlSin(object a)
    {
      return Math.Sin(RequiresNotNull<double>(a));
    }

    //(flcos fl) procedure
    [Builtin("flcos")]
    public static object FlCos(object a)
    {
      return Math.Cos(RequiresNotNull<double>(a));
    }

    //(fltan fl) procedure
    [Builtin("fltan")]
    public static object FlTan(object a)
    {
      return Math.Tan(RequiresNotNull<double>(a));
    }

    //(flasin fl) procedure
    [Builtin("flasin")]
    public static object FlAsin(object a)
    {
      return Math.Asin(RequiresNotNull<double>(a));
    }

    //(flacos fl) procedure
    [Builtin("flacos")]
    public static object FlAcos(object a)
    {
      return Math.Acos(RequiresNotNull<double>(a));
    }

    //(flatan fl) procedure
    [Builtin("flatan")]
    public static object FlAtan(object a)
    {
      return Math.Atan(RequiresNotNull<double>(a));
    }

    //(flatan fl1 fl2) procedure
    [Builtin("flatan")]
    public static object FlAtan(object a, object b)
    {
      return Math.Atan2(RequiresNotNull<double>(a), RequiresNotNull<double>(b));
    }

    //(flsqrt fl)
    [Builtin("flsqrt")]
    public static object FlSqrt(object a)
    {
      return Math.Sqrt(RequiresNotNull<double>(a));
    }

    //(flexpt fl1 fl2)
    [Builtin("flexpt")]
    public static object FlExpt(object a, object b)
    {
      return Math.Pow(RequiresNotNull<double>(a), RequiresNotNull<double>(b));
    }

    //(fixnum->flonum fx)
    [Builtin("fixnum->flonum")]
    public static object FixnumToFlonum(object a)
    {
      return (double)RequiresNotNull<int>(a);
    }
  }
}
