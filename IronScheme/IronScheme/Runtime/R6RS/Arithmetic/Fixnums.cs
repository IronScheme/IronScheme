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
using Microsoft.Scripting.Ast;
using IronScheme.Compiler;
using Microsoft.Scripting;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Math;
using System.Collections.Specialized;

namespace IronScheme.Runtime.R6RS.Arithmetic
{
  static class FixnumsInlineEmitters
  {
    static bool Expect(Expression[] args, int count)
    {
      return args.Length == count &&
        Array.TrueForAll(args, delegate(Expression e) { return e.Type == typeof(object) || e.Type == typeof(int); });
    }

    [InlineEmitter("fixnum-width")]
    public static Expression FixnumWidth(params Expression[] args)
    {
      if (Expect(args, 0))
      {
        return Ast.Constant(32);
      }
      return null;
    }

    [InlineEmitter("greatest-fixnum")]
    public static Expression GreatestFixnum(params Expression[] args)
    {
      if (Expect(args, 0))
      {
        return Ast.Constant(int.MaxValue);
      }
      return null;
    }

    [InlineEmitter("least-fixnum")]
    public static Expression LeastFixnum(params Expression[] args)
    {
      if (Expect(args, 0))
      {
        return Ast.Constant(int.MinValue);
      }
      return null;
    }

    [InlineEmitter("fx=?")]
    public static Expression FxEquals(params Expression[] args)
    {
      if (Expect(args, 2))
      {
        return Ast.Equal(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
      return null;
    }

    // cant deal effectively with .NET exceptions :(, dont care...
    [InlineEmitter("fx+", Optimization = OptimizationLevel.Safe)]
    public static Expression FxAddChecked(params Expression[] args)
    {
      if (Expect(args, 2))
      {
        return Ast.AddChecked(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
      return null;
    }

    [InlineEmitter("fx*", Optimization = OptimizationLevel.Safe)]
    public static Expression FxMultiplyChecked(params Expression[] args)
    {
      if (Expect(args, 2))
      {
        return Ast.MultiplyChecked(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
      return null;
    }

    [InlineEmitter("fx-", Optimization = OptimizationLevel.Safe)]
    public static Expression FxMinusChecked(params Expression[] args)
    {
      if (args.Length == 1)
      {
        return Ast.Negate(UnwrapAndCast<int>(args[0]));
      }
      else
      {
        if (Expect(args, 2))
        {
          return Ast.SubtractChecked(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }
    }

    [InlineEmitter("fx+", Optimization = OptimizationLevel.Unchecked)]
    public static Expression FxAdd(params Expression[] args)
    {
      if (Expect(args, 2))
      {
        return Ast.Add(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
      return null;
    }

    [InlineEmitter("fx*", Optimization = OptimizationLevel.Unchecked)]
    public static Expression FxMultiply(params Expression[] args)
    {
      if (Expect(args, 2))
      {
        return Ast.Multiply(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
      return null;
    }

    [InlineEmitter("fx-", Optimization = OptimizationLevel.Unchecked)]
    public static Expression FxMinus(params Expression[] args)
    {
      if (args.Length == 1)
      {
        return Ast.Negate(UnwrapAndCast<int>(args[0]));
      }
      else
      {
        if (Expect(args, 2))
        {
          return Ast.Subtract(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }
    }

    [InlineEmitter("fxnot", Optimization = OptimizationLevel.Unchecked)]
    public static Expression FxNot(params Expression[] args)
    {
      if (Expect(args, 1))
      {
        return Ast.Not(UnwrapAndCast<int>(args[0]));
      }
      return null;
    }

    [InlineEmitter("fx=?", Optimization = OptimizationLevel.Safe)]
    public static Expression FxIsEqual(params Expression[] args)
    {
      if (Expect(args, 2))
      {
        return Ast.Equal(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
      return null;
    }

    [InlineEmitter("fx<?", Optimization = OptimizationLevel.Safe)]
    public static Expression FxIsLessThan(params Expression[] args)
    {
      if (Expect(args, 2))
      {
        return Ast.LessThan(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
      return null;
    }

    [InlineEmitter("fx<=?", Optimization = OptimizationLevel.Safe)]
    public static Expression FxIsLessThanOrEqual(params Expression[] args)
    {
      if (Expect(args, 2))
      {
        return Ast.LessThanEquals(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
      return null;
    }

    [InlineEmitter("fx>?", Optimization = OptimizationLevel.Safe)]
    public static Expression FxIsGreater(params Expression[] args)
    {
      if (Expect(args, 2))
      {
        return Ast.GreaterThan(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
      return null;
    }

    [InlineEmitter("fx>=?", Optimization = OptimizationLevel.Safe)]
    public static Expression FxIsGreaterOrEqual(params Expression[] args)
    {
      if (Expect(args, 2))
      {
        return Ast.GreaterThanEquals(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
      return null;
    }

    [InlineEmitter("fxand", Optimization = OptimizationLevel.Safe)]
    public static Expression FxAnd(params Expression[] args)
    {
      if (Expect(args, 2))
      {
        return Ast.Add(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
      return null;
    }

    [InlineEmitter("fxior", Optimization = OptimizationLevel.Safe)]
    public static Expression FxIor(params Expression[] args)
    {
      if (Expect(args, 2))
      {
        return Ast.Or(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
      return null;
    }

    [InlineEmitter("fxxor", Optimization = OptimizationLevel.Safe)]
    public static Expression FxXor(params Expression[] args)
    {
      if (Expect(args, 2))
      {
        return Ast.ExclusiveOr(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
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

    static Expression UnwrapAndCast<T>(Expression e)
    {
      e = Unwrap(e);
      if (e.Type == typeof(object) || e.Type == typeof(T))
      {
        return Ast.ConvertHelper(Unwrap(e), typeof(T));
      }
      Builtins.SyntaxError("UnwrapAndCast", "Expected fixnum, but got " + e.Type.Name + ".", e, false);
      return null;
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

  public class Fixnums : Builtins
  {
    [Builtin("greatest-fixnum")]
    public static object GreatestFixnum()
    {
      return int.MaxValue;
    }

    [Builtin("least-fixnum")]
    public static object LeastFixnum()
    {
      return int.MinValue;
    }

    static void ExpectAtLeast(object[] args, int len)
    {
      if (args.Length < len)
      {
        AssertionViolation(GetCaller(), "Expected at least " + len + " arguments, but got " + args.Length + ".", args);
      }
    }

    [Builtin("fx=?")]
    public static object FxEqual(object first, object second)
    {
      return GetBool(RequiresNotNull<int>(first) == RequiresNotNull<int>(second));
    }


    [Builtin("fx=?")]
    public static object FxEqual(params object[] all)
    {
      ExpectAtLeast(all, 2);
      for (int i = 0; i < all.Length - 1; i++)
      {
        if ((int)all[i] != (int)all[i + 1])
        {
          return FALSE;
        }
      }
      return TRUE;
    }

    [Builtin("fx>?")]
    public static object FxGreater(params object[] all)
    {
      ExpectAtLeast(all, 2);
      for (int i = 0; i < all.Length - 1; i++)
      {
        if (!((int)all[i] > (int)all[i + 1]))
        {
          return FALSE;
        }
      }
      return TRUE;
    }


    [Builtin("fx<?")]
    public static object FxLess(params object[] all)
    {
      ExpectAtLeast(all, 2);
      for (int i = 0; i < all.Length - 1; i++)
      {
        if (!((int)all[i] < (int)all[i + 1]))
        {
          return FALSE;
        }
      }
      return TRUE;
    }

    [Builtin("fx>=?")]
    public static object FxGreaterOrEqual(params object[] all)
    {
      ExpectAtLeast(all, 2);
      for (int i = 0; i < all.Length - 1; i++)
      {
        if (!((int)all[i] >= (int)all[i + 1]))
        {
          return FALSE;
        }
      }
      return TRUE;
    }

    [Builtin("fx<=?")]
    public static object FxLessOrEqual(params object[] all)
    {
      ExpectAtLeast(all, 2);
      for (int i = 0; i < all.Length - 1; i++)
      {
        if (!((int)all[i] <= (int)all[i + 1]))
        {
          return FALSE;
        }
      }
      return TRUE;
    }

    [Builtin("fx+")]
    public static object FxAdd(object a, object b)
    {
      int x1 = RequiresNotNull<int>(a);
      int x2 = RequiresNotNull<int>(b);

      try
      {
        return checked(x1 + x2);
      }
      catch (OverflowException)
      {
        return ImplementationRestriction("fx+", "arithmetic overflow", a , b);
      }
    }

    [Builtin("fx*")]
    public static object FxMultiply(object a, object b)
    {
      int x1 = RequiresNotNull<int>(a);
      int x2 = RequiresNotNull<int>(b);

      try
      {
        return checked(x1 * x2);
      }
      catch (OverflowException)
      {
        return ImplementationRestriction("fx*", "arithmetic overflow", a, b);
      }
    }

    [Builtin("fx-")]
    public static object FxMinus(object a)
    {
      int x1 = RequiresNotNull<int>(a);

      // this try is dumb, only 1 case i think
      try
      {
        return checked(-x1);
      }
      catch (OverflowException)
      {
        return ImplementationRestriction("fx-", "arithmetic overflow", a);
      }
    }

    [Builtin("fx-")]
    public static object FxMinus(object a, object b)
    {
      int x1 = RequiresNotNull<int>(a);
      int x2 = RequiresNotNull<int>(b);

      try
      {
        return checked(x1 - x2);
      }
      catch (OverflowException)
      {
        return ImplementationRestriction("fx-", "arithmetic overflow", a, b);
      }
    }

    [Builtin("fxdiv")]
    public static object FxDiv(object x1, object x2)
    {
      int a = RequiresNotNull<int>(x1);
      int b = RequiresNotNull<int>(x2);

      if (b == 0)
      {
        return AssertionViolation("fxdiv", "divide by zero", x1, x2);
      }

      int div;

      if (a == 0)
      {
        div = 0;
      }
      else if (a > 0)
      {
        div = a / b;
      }
      else if (b > 0)
      {
        div = (a - b + 1) / b;
      }
      else
      {
        try
        {
          div = (a + b + 1) / b;
        }
        catch (OverflowException)
        {
          return AssertionViolation("fxdiv", "result outside fixnum range", x1, x2);
        }

      }

      return div;
    }

    [Builtin("fxdiv0")]
    public static object FxDiv0(object x1, object x2)
    {
      object div = FxDiv(x1, x2);
      object mod = Subtract(x1, Multiply(div, x2));

      if (IsTrue(IsLessThan(mod, Magnitude(Divide(x2, 2)))))
      {
        return div;
      }
      if (IsTrue(IsPositive(x2)))
      {
        return FxAdd(div, 1);
      }
      return FxMinus(div, 1);
    }



    [Builtin("fxdiv-and-mod")]
    public static object FxDivMod(object x1, object x2)
    {
      int a = RequiresNotNull<int>(x1);
      int b = RequiresNotNull<int>(x2);

      if (b == 0)
      {
        return AssertionViolation("fxdiv-and-mod", "divide by zero", x1, x2);
      }


      int div = a / b;
      int mod = a % b;

      if (div < 0)
      {
        div--;
      }

      if (mod < 0)
      {
        mod += (b * Math.Sign(b));
        if ((a > 0 && b > 0) || (a < 0 && b < 0))
        {
          div++;
        }
      }
      else if (mod > b)
      {
        if (!((a > 0 && b > 0) || (a < 0 && b < 0)))
        {
          div++;
        }
      }
      return Values(div, mod);
    }

    [Builtin("fxdiv0-and-mod0")]
    public static object FxDiv0Mod0(object x1, object x2)
    {
      int a = RequiresNotNull<int>(x1);
      int b = RequiresNotNull<int>(x2);

      MultipleValues dv = (MultipleValues)FxDivMod(x1, x2);
      int div = (int)dv[0];
      int mod = (int)dv[1];
      int h = b / 2;

      if (mod > h && mod > -h)
      {
        mod -= (b * Math.Sign(b));
        if (((a > 0 && b > 0) || (a < 0 && b < 0)) && mod != -h)
        {
          div--;
        }
        else
        {
          div++;
        }
      }
      else if (mod == h)
      {
        mod -= (b * Math.Sign(b));
        div++;
      }

      return Values(div, mod);
    }

    [Builtin("fxnot")]
    public static object FxNot(object a)
    {
      return ~RequiresNotNull<int>(a);
    }

    [Builtin("fxand")]
    public static object FxAnd(params object[] args)
    {
      int arglen = args.Length;
      switch (arglen)
      {
        case 0:
          return -1;
        case 1:
          return RequiresNotNull<int>(args[0]);
        case 2:
          return RequiresNotNull<int>(args[0]) & RequiresNotNull<int>(args[1]);
        default:
          object[] head = ArrayUtils.RemoveLast(args);
          return FxAnd(FxAnd(head), args[arglen - 1]);
      }
    }

    [Builtin("fxior")]
    public static object FxIor(params object[] args)
    {
      int arglen = args.Length;
      switch (arglen)
      {
        case 0:
          return 0;
        case 1:
          return RequiresNotNull<int>(args[0]);
        case 2:
          return RequiresNotNull<int>(args[0]) | RequiresNotNull<int>(args[1]);
        default:
          object[] head = ArrayUtils.RemoveLast(args);
          return FxIor(FxIor(head), args[arglen - 1]);
      }
    }

    [Builtin("fxxor")]
    public static object FxXor(params object[] args)
    {
      int arglen = args.Length;
      switch (arglen)
      {
        case 0:
          return 0;
        case 1:
          return RequiresNotNull<int>(args[0]);
        case 2:
          return RequiresNotNull<int>(args[0]) ^ RequiresNotNull<int>(args[1]);
        default:
          object[] head = ArrayUtils.RemoveLast(args);
          return FxXor(FxXor(head), args[arglen - 1]);
      }
    }



    //(fxbit-count fx)
    [Builtin("fxbit-count")]
    public static object FxBitCount(object ei)
    {
      int bi = RequiresNotNull<int>(ei);

      if (bi <= 0)
      {
        return FxNot(FxBitCount(FxNot(ei)));
      }
      else
      {
        int count = 0;
        while (bi > 0)
        {
          count += (int)(bi & 1);
          bi >>= 1;
        }
        return count;
      }
    }

    //(fxlength fx)
    [Builtin("fxlength")]
    public static object FxLength(object ei)
    {
      int bi = RequiresNotNull<int>(ei);

      if (bi < 0)
      {
        return FxLength(FxNot(ei));
      }
      else
      {
        int count = 0;
        while (bi > 0)
        {
          count++;
          bi >>= 1;
        }
        return count;
      }
    }

    //(fxfirst-bit-set fx)
    [Builtin("fxfirst-bit-set")]
    public static object FxFirstBitSet(object ei)
    {
      int bi = RequiresNotNull<int>(ei);

      if (bi == 0)
      {
        return -1;
      }
      else
      {
        int count = 0;
        while (bi != 0)
        {
          if ((int)(bi & 1) == 1)
          {
            return count;
          }
          count++;
          bi >>= 1;
        }
        return count;
      }
    }

    //(fxbit-set? fx1 fx2)
    [Builtin("fxbit-set?")]
    public static object FxIsBitSet(object ei, object k)
    {
      int bi = RequiresNotNull<int>(ei);
      int ki = RequiresNotNull<int>(k);

      if (ki < 0)
      {
        return AssertionViolation(SymbolTable.StringToId("fx-bit-set?"), "k is negative", k);
      }
      else if (ki >= 32)
      {
        return AssertionViolation(SymbolTable.StringToId("fx-bit-set?"), "k must be less than fixnum width", k);
      }

      if (bi == 0)
      {
        return FALSE;
      }
      else
      {
        int count = 0;
        while (bi != 0)
        {
          if ((int)(bi & 1) == 1 && count == ki)
          {
            return TRUE;
          }
          count++;
          bi >>= 1;
        }
        return FALSE;
      }
    }

    //(fxarithmetic-shift fx1 fx2)
    [Builtin("fxarithmetic-shift")]
    public static object FxArithmeticShift(object ei, object k)
    {
      int bi = RequiresNotNull<int>(ei);
      int ki = RequiresNotNull<int>(k);

      if (ki == 0)
      {
        return ei;
      }
      if (ki < 0)
      {
        return bi >> -ki;
      }
      else
      {
        int i = bi << ki;
        if (Math.Abs(bi) > Math.Abs(i))
        {
          return ImplementationRestriction("fxarithmetic-shift", "arithmetic overflow", ei, k);
        }
        return i;
      }
    }

    //(fxreverse-bit-field fx1 fx2 fx3)
    [Builtin("fxreverse-bit-field")]
    public static object FxReverseBitField(object fx1, object fx2, object fx3)
    {
      int i1 = RequiresNotNull<int>(fx1);
      int i2 = RequiresNotNull<int>(fx2);
      int i3 = RequiresNotNull<int>(fx3);

      if (i2 >= i3)
      {
        return AssertionViolation("fxreverse-bit-field", "start must be less than end", fx2, fx3);
      }

      BitVector32 bitvec = new BitVector32(i1);

      int range = i3 - i2 - 1;

      for (int i = i2; i < (i3 - range/2); i++)
      {
        int m1 = BitVector32.CreateMask(i);
        int m2 = BitVector32.CreateMask(i3 - (i - i2));
        bool b1 = bitvec[m1];
        bool b2 = bitvec[m2];
        bitvec[m1] = b2;
        bitvec[m2] = b1;
      }

      return bitvec.Data;
    }

  }
}
