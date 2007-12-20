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
    static void Expect(Expression[] args, int count)
    {
      if (args.Length != count)
      {
        throw new ArgumentException("Expected " + count + " arguments. Got " + args.Length + " arguments.");
      }
    }

    [InlineEmitter("flonum?")]
    public static Expression IsFlonum(params Expression[] args)
    {
      Expect(args, 1);
      return Ast.TypeIs(Unwrap(args[0]), typeof(double));
    }

    [InlineEmitter("flonum-width")]
    public static Expression FlonumWidth(params Expression[] args)
    {
      Expect(args, 0);
      return Ast.Constant(64);
    }

    [InlineEmitter("greatest-flonum")]
    public static Expression GreatestFlonum(params Expression[] args)
    {
      Expect(args, 0);
      return Ast.Constant(double.MaxValue);
    }

    [InlineEmitter("least-flonum")]
    public static Expression LeastFlonum(params Expression[] args)
    {
      Expect(args, 0);
      return Ast.Constant(double.MinValue);
    }

    [InlineEmitter("fl=?")]
    public static Expression FlEqual(params Expression[] args)
    {
      if (args.Length == 2)
      {
        return Ast.Equal(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
      }
      else
      {
        if (IsConstant(UnwrapAndCast<double>(args)))
        {
          Expression[] rest = ArrayUtils.RemoveFirst(args);
          return Ast.AndAlso(FlEqual(args[0], args[1]), FlEqual(rest));
        }
        return null;
      }
    }


    [InlineEmitter("fl>?")]
    public static Expression FlGreater(params Expression[] args)
    {
      if (args.Length == 2)
      {
        return Ast.GreaterThan(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
      }
      else
      {
        if (IsConstant(UnwrapAndCast<double>(args)))
        {
          Expression[] rest = ArrayUtils.RemoveFirst(args);
          return Ast.AndAlso(FlGreater(args[0], args[1]), FlGreater(rest));
        }
        return null;
      }
    }

    [InlineEmitter("fl<?")]
    public static Expression FlLess(params Expression[] args)
    {
      if (args.Length == 2)
      {
        return Ast.LessThan(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
      }
      else
      {
        if (IsConstant(UnwrapAndCast<double>(args)))
        {
          Expression[] rest = ArrayUtils.RemoveFirst(args);
          return Ast.AndAlso(FlLess(args[0], args[1]), FlLess(rest));
        }
        return null;
      }
    }


    [InlineEmitter("fl>=?")]
    public static Expression FlGreaterOrEqual(params Expression[] args)
    {
      if (args.Length == 2)
      {
        return Ast.GreaterThanEquals(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
      }
      else
      {
        if (IsConstant(UnwrapAndCast<double>(args)))
        {
          Expression[] rest = ArrayUtils.RemoveFirst(args);
          return Ast.AndAlso(FlGreaterOrEqual(args[0], args[1]), FlGreaterOrEqual(rest));
        }
        return null;
      }
    }

    [InlineEmitter("fl<=?")]
    public static Expression FlLessOrEqual(params Expression[] args)
    {
      if (args.Length == 2)
      {
        return Ast.LessThanEquals(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
      }
      else
      {
        if (IsConstant(UnwrapAndCast<double>(args)))
        {
          Expression[] rest = ArrayUtils.RemoveFirst(args);
          return Ast.AndAlso(FlLessOrEqual(args[0], args[1]), FlLessOrEqual(rest));
        }

        return null;
      }
    }

    [InlineEmitter("fl+")]
    public static Expression FlAdd(params Expression[] args)
    {
      int arglen = args.Length;
      switch (arglen)
      {
        case 0:
          return Ast.Constant(0.0);
        case 1:
          return UnwrapAndCast<double>(args[0]);
        case 2:
          return Ast.Add(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        default:
          Expression[] rest = ArrayUtils.RemoveFirst(args);
          return Ast.Add(UnwrapAndCast<double>(args[0]), FlAdd(rest));
      }
    }

    [InlineEmitter("fl*")]
    public static Expression FlMultiply(params Expression[] args)
    {
      int arglen = args.Length;
      switch (arglen)
      {
        case 0:
          return Ast.Constant(1.0);
        case 1:
          return UnwrapAndCast<double>(args[0]);
        case 2:
          return Ast.Multiply(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        default:
          Expression[] rest = ArrayUtils.RemoveFirst(args);
          return Ast.Multiply(UnwrapAndCast<double>(args[0]), FlMultiply(rest));
      }
    }

    [InlineEmitter("fl-")]
    public static Expression FlMinus(params Expression[] args)
    {
      if (args.Length == 1)
      {
        return Ast.Negate(UnwrapAndCast<double>(args[0]));
      }
      else
      {
        if (args.Length == 2)
        {
          return Ast.Subtract(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        }
        else
        {
          Expression[] rest = ArrayUtils.RemoveFirst(args);
          return Ast.Subtract(UnwrapAndCast<double>(args[0]), FlMinus(rest));
        }
      }
    }


    [InlineEmitter("fldiv")]
    public static Expression FlDiv(params Expression[] args)
    {
      Expect(args, 2);
      return Ast.Divide(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
    }


    [InlineEmitter("flmod")]
    public static Expression FlMod(params Expression[] args)
    {
      Expect(args, 2);
      return Ast.Modulo(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
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

    static Expression UnwrapAndCast<T>(Expression e)
    {
      e = Unwrap(e);
      if (e.Type == typeof(object) || e.Type == typeof(T))
      {
        return Ast.ConvertHelper(Unwrap(e), typeof(T));
      }
      throw new ArgumentTypeException("Expected flonum, but got " + e.Type.Name + ".");
    }

    static Expression[] Unwrap(Expression[] ee)
    {
      for (int i = 0; i < ee.Length; i++)
      {
        ee[i] = Unwrap(ee[i]);
      }
      return ee;
    }

    static Expression[] UnwrapAndCast<T>(Expression[] ee)
    {
      for (int i = 0; i < ee.Length; i++)
      {
        ee[i] = UnwrapAndCast<T>(ee[i]);
      }
      return ee;
    }


    static bool IsConstant(Expression[] args)
    {
      for (int i = 0; i < args.Length; i++)
      {
        if (!(args[i] is ConstantExpression))
        {
          return false;
        }
      }
      return true;
    }


  }

  public class Flonums : Builtins
  {
    [Builtin("flonum?")]
    public static object IsFlonum(object o)
    {
      return o is double;
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
        throw new ArgumentException("Expected at least " + len + " arguments, but got " + args.Length + ".");
      }
    }

    [Builtin("real->flonum")]
    public static object RealToFlonum(object n)
    {
      return Convert.ToDouble(n);
    }


    [Builtin("fl=?")]
    public static object FlEqual(params object[] all)
    {
      ExpectAtLeast(all, 2);
      for (int i = 0; i < all.Length - 1; i++)
      {
        if ((double)all[i] != (double)all[i + 1])
        {
          return false;
        }
      }
      return true;
    }

    [Builtin("fl>?")]
    public static object FlGreater(params object[] all)
    {
      ExpectAtLeast(all, 2);
      for (int i = 0; i < all.Length - 1; i++)
      {
        if (!((double)all[i] > (double)all[i + 1]))
        {
          return false;
        }
      }
      return true;
    }


    [Builtin("fl<?")]
    public static object FlLess(params object[] all)
    {
      ExpectAtLeast(all, 2);
      for (int i = 0; i < all.Length - 1; i++)
      {
        if (!((double)all[i] < (double)all[i + 1]))
        {
          return false;
        }
      }
      return true;
    }

    [Builtin("fl>=?")]
    public static object FlGreaterOrEqual(params object[] all)
    {
      ExpectAtLeast(all, 2);
      for (int i = 0; i < all.Length - 1; i++)
      {
        if (!((double)all[i] >= (double)all[i + 1]))
        {
          return false;
        }
      }
      return true;
    }


    [Builtin("fl<=?")]
    public static object FlLessOrEqual(params object[] all)
    {
      ExpectAtLeast(all, 2);
      for (int i = 0; i < all.Length - 1; i++)
      {
        if (!((double)all[i] <= (double)all[i + 1]))
        {
          return false;
        }
      }
      return true;
    }

    [Builtin("flinteger?")]
    public static object FlIsInteger(object a)
    {
      return (double)a%1.0 == 0;
    }

    [Builtin("flzero?")]
    public static object FlIsZero(object a)
    {
      return (double)a == 0;
    }

    [Builtin("flpositive?")]
    public static object FlIsPositive(object a)
    {
      return (double)a > 0;
    }

    [Builtin("flnegative?")]
    public static object FlIsNegative(object a)
    {
      return (double)a < 0;
    }

    [Builtin("flodd?")]
    public static object FlIsOdd(object a)
    {
      return (double)a % 2 == 1;
    }

    [Builtin("fleven?")]
    public static object FlIsEven(object a)
    {
      return (double)a % 2 == 0;
    }

    [Builtin("flfinite?")]
    public static object FlIsFinite(object a)
    {
      return !double.IsInfinity( (double)a);
    }

    [Builtin("flinfinite?")]
    public static object FlIsInfinite(object a)
    {
      return double.IsInfinity((double)a);
    }

    [Builtin("flnan?")]
    public static object FlIsNan(object a)
    {
      return double.IsNaN((double)a);
    }


    [Builtin("flmax")]
    public static object FlMax(params object[] args)
    {
      ExpectAtLeast(args, 1);
      int arglen = args.Length;
      switch (arglen)
      {
        case 1:
          return args[0];
        case 2:
          return Math.Max((double)args[0], (double)args[1]);
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
          return args[0];
        case 2:
          return Math.Min((double)args[0], (double)args[1]);
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
      return (double)a;
    }


    [Builtin("fl+")]
    public static object FlAdd(object a, object b)
    {
      return (double)a + (double)b;
    }

    [Builtin("fl+")]
    public static object FlAdd(params object[] all)
    {
      double result = 0.0;
      for (int i = 0; i < all.Length; i++)
      {
        result += (double)all[i];
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
      return (double)a;
    }

    [Builtin("fl*")]
    public static object FlMultiply(object a, object b)
    {
      return (double)a * (double)b;
    }

    [Builtin("fl*")]
    public static object FlMultiply(params object[] all)
    {
      double result = 1.0;
      for (int i = 0; i < all.Length; i++)
      {
        result *= (double)all[i];
      }
      return result;
    }

    [Builtin("fl-")]
    public static object FlMinus(object a)
    {
      return -(double)a;
    }

    [Builtin("fl-")]
    public static object FlMinus(object a, object b)
    {
      return (double)a - (double)b;
    }

    [Builtin("fl-")]
    public static object FlMinus(params object[] all)
    {
      ExpectAtLeast(all, 1);

      double result = (double)all[0];
      for (int i = 1; i < all.Length; i++)
      {
        result -= (double)all[i];
      }
      return result;
    }

    [Builtin("fl/")]
    public static object FlQuotient(object a)
    {
      return 1.0/(double)a;
    }

    [Builtin("fl/")]
    public static object FlQuotient(object a, object b)
    {
      return (double)a / (double)b;
    }

    [Builtin("fl/")]
    public static object FlQuotient(params object[] all)
    {
      ExpectAtLeast(all, 1);

      double result = (double)all[0];
      for (int i = 1; i < all.Length; i++)
      {
        result /= (double)all[i];
      }
      return result;
    }

    [Builtin("flabs")]
    public static object FlAbs(object a)
    {
      return Math.Abs((double)a);
    }

    [Builtin("fldiv-and-mod")]
    public static object FlDivAndMod(object a, object b)
    {
      double x1 = RequiresNotNull<double>(a);
      double x2 = RequiresNotNull<double>(b);

      object[] r = (object[]) DivMod(x1, x2);
      r[0] = Convert.ToDouble(r[0]);

      return r;
    }

    [Builtin("fldiv")]
    public static object FlDiv(object a, object b)
    {
      return ((object[])FlDivAndMod(a, b))[0];
    }


    [Builtin("flmod")]
    public static object FlMod(object a, object b)
    {
      return ((object[])FlDivAndMod(a, b))[1];
    }

    [Builtin("fldiv0-and-mod0")]
    public static object FlDiv0AndMod0(object a, object b)
    {
      double x1 = RequiresNotNull<double>(a);
      double x2 = RequiresNotNull<double>(b);

      object[] r = (object[])Div0Mod0(x1, x2);
      r[0] = Convert.ToDouble(r[0]);

      return r;

    }

    [Builtin("fldiv0")]
    public static object FlDiv0(object a, object b)
    {
      return ((object[])FlDiv0AndMod0(a, b))[0];
    }


    [Builtin("flmod0")]
    public static object FlMod0(object a, object b)
    {
      return ((object[])FlDiv0AndMod0(a, b))[1];
    }

    //(flnumerator fl) procedure
    //(fldenominator fl)

    //(flfloor fl) procedure
    [Builtin("flfloor")]
    public static object FlFloor(object a)
    {
      return Math.Floor((double)a);
    }

    //(flceiling fl) procedure
    [Builtin("flceiling")]
    public static object FlCeiling(object a)
    {
      return Math.Ceiling((double)a);
    }

    //(fltruncate fl) procedure
    [Builtin("fltruncate")]
    public static object FlTruncate(object a)
    {
      return Math.Truncate((double)a);
    }

    //(flround fl)
    [Builtin("flround")]
    public static object FlRound(object a)
    {
      return Math.Round((double)a);
    }

    //(flexp fl) procedure
    [Builtin("flexp")]
    public static object FlExp(object a)
    {
      return Math.Exp((double)a);
    }

    //(fllog fl) procedure
    [Builtin("fllog")]
    public static object FlLog(object a)
    {
      return Math.Log((double)a);
    }

    //(fllog fl1 fl2) procedure
    [Builtin("fllog")]
    public static object FlLog(object a, object b)
    {
      return Math.Log((double)a, (double)b);
    }

    //(flsin fl) procedure
    [Builtin("flsin")]
    public static object FlSin(object a)
    {
      return Math.Sin((double)a);
    }

    //(flcos fl) procedure
    [Builtin("flcos")]
    public static object FlCos(object a)
    {
      return Math.Cos((double)a);
    }

    //(fltan fl) procedure
    [Builtin("fltan")]
    public static object FlTan(object a)
    {
      return Math.Tan((double)a);
    }

    //(flasin fl) procedure
    [Builtin("flasin")]
    public static object FlAsin(object a)
    {
      return Math.Asin((double)a);
    }

    //(flacos fl) procedure
    [Builtin("flacos")]
    public static object FlAcos(object a)
    {
      return Math.Acos((double)a);
    }

    //(flatan fl) procedure
    [Builtin("flatan")]
    public static object FlAtan(object a)
    {
      return Math.Atan((double)a);
    }

    //(flatan fl1 fl2) procedure
    [Builtin("flatan")]
    public static object FlAtan(object a, object b)
    {
      return Math.Atan2((double)a, (double)b);
    }

    //(flsqrt fl)
    [Builtin("flsqrt")]
    public static object FlSqrt(object a)
    {
      return Math.Sqrt((double)a);
    }

    //(flexpt fl1 fl2)
    [Builtin("flexpt")]
    public static object FlExpt(object a, object b)
    {
      return Math.Pow((double)a, (double)b);
    }

    //(fixnum->flonum fx)
    [Builtin("fixnum->flonum")]
    public static object FixnumToFlonum(object a)
    {
      return (double)(int)a;
    }
  }
}
