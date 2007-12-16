using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Ast;
using IronScheme.Compiler;
using Microsoft.Scripting;
using Microsoft.Scripting.Utils;

namespace IronScheme.Runtime.R6RS.Arithmetic
{
  static class FixnumsInlineEmitters
  {
    static void Expect(Expression[] args, int count)
    {
      if (args.Length != count)
      {
        throw new ArgumentException("Expected " + count + " arguments. Got " + args.Length + " arguments.");
      }
    }

    [InlineEmitter("fixnum?")]
    public static Expression IsFixnum(params Expression[] args)
    {
      Expect(args, 1);
      return Ast.TypeIs(Unwrap(args[0]), typeof(int));
    }

    [InlineEmitter("fixnum-width")]
    public static Expression FixnumWidth(params Expression[] args)
    {
      Expect(args, 0);
      return Ast.Constant(32);
    }

    [InlineEmitter("greatest-fixnum")]
    public static Expression GreatestFixnum(params Expression[] args)
    {
      Expect(args, 0);
      return Ast.Constant(int.MaxValue);
    }

    [InlineEmitter("least-fixnum")]
    public static Expression LeastFixnum(params Expression[] args)
    {
      Expect(args, 0);
      return Ast.Constant(int.MinValue);
    }

    [InlineEmitter("fx=?")]
    public static Expression FxEqual(params Expression[] args)
    {
      if (args.Length == 2)
      {
        return Ast.Equal(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
      else
      {
        if (IsConstant(UnwrapAndCast<int>(args)))
        {
          Expression[] rest = ArrayUtils.RemoveFirst(args);
          return Ast.AndAlso(FxEqual(args[0], args[1]), FxEqual(rest));
        }
        return null;
      }
    }


    [InlineEmitter("fx>?")]
    public static Expression FxGreater(params Expression[] args)
    {
      if (args.Length == 2)
      {
        return Ast.GreaterThan(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
      else
      {
        if (IsConstant(UnwrapAndCast<int>(args)))
        {
          Expression[] rest = ArrayUtils.RemoveFirst(args);
          return Ast.AndAlso(FxGreater(args[0], args[1]), FxGreater(rest));
        }
        return null;
      }
    }

    [InlineEmitter("fx<?")]
    public static Expression FxLess(params Expression[] args)
    {
      if (args.Length == 2)
      {
        return Ast.LessThan(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
      else
      {
        if (IsConstant(UnwrapAndCast<int>(args)))
        {
          Expression[] rest = ArrayUtils.RemoveFirst(args);
          return Ast.AndAlso(FxLess(args[0], args[1]), FxLess(rest));
        }
        return null;
      }
    }


    [InlineEmitter("fx>=?")]
    public static Expression FxGreaterOrEqual(params Expression[] args)
    {
      if (args.Length == 2)
      {
        return Ast.GreaterThanEquals(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
      else
      {
        if (IsConstant(UnwrapAndCast<int>(args)))
        {
          Expression[] rest = ArrayUtils.RemoveFirst(args);
          return Ast.AndAlso(FxGreaterOrEqual(args[0], args[1]), FxGreaterOrEqual(rest));
        }
        return null;
      }
    }

    [InlineEmitter("fx<=?")]
    public static Expression FxLessOrEqual(params Expression[] args)
    {
      if (args.Length == 2)
      {
        return Ast.LessThanEquals(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
      else
      {
        if (IsConstant(UnwrapAndCast<int>(args)))
        {
          Expression[] rest = ArrayUtils.RemoveFirst(args);
          return Ast.AndAlso(FxLessOrEqual(args[0], args[1]), FxLessOrEqual(rest));
        }

        return null;
      }
    }

    [InlineEmitter("fx+")]
    public static Expression FxAdd(params Expression[] args)
    {
      Expect(args, 2);
      return Ast.Add(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
    }

    [InlineEmitter("fx*")]
    public static Expression FxMultiply(params Expression[] args)
    {
      Expect(args, 2);
      return Ast.Multiply(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
    }

    [InlineEmitter("fx-")]
    public static Expression FxMinus(params Expression[] args)
    {
      if (args.Length == 1)
      {
        return Ast.Negate(UnwrapAndCast<int>(args[0]));
      }
      else
      {
        Expect(args, 2);
        return Ast.Subtract(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
      }
    }


    [InlineEmitter("fxdiv")]
    public static Expression FxDiv(params Expression[] args)
    {
      Expect(args, 2);
      return Ast.Divide(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
    }


    [InlineEmitter("fxmod")]
    public static Expression FxMod(params Expression[] args)
    {
      Expect(args, 2);
      return Ast.Modulo(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
    }

    [InlineEmitter("fxnot")]
    public static Expression FxNot(params Expression[] args)
    {
      Expect(args, 1);
      return Ast.Not(UnwrapAndCast<int>(args[0]));
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
      throw new ArgumentTypeException("Expected fixnum, but got " + e.Type.Name + ".");
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
    [Builtin("fixnum?")]
    public static object IsFixnum(object o)
    {
      return o is int;
    }

    [Builtin("fixnum-width")]
    public static object FixnumWidth()
    {
      return 32;
    }

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
        throw new ArgumentException("Expected at least " + len + " arguments, but got " + args.Length + ".");
      }
    }

    [Builtin("fx=?")]
    public static object FxEqual(params object[] all)
    {
      ExpectAtLeast(all, 2);
      for (int i = 0; i < all.Length - 1; i++)
      {
        if ((int)all[i] != (int)all[i + 1])
        {
          return false;
        }
      }
      return true;
    }

    [Builtin("fx>?")]
    public static object FxGreater(params object[] all)
    {
      ExpectAtLeast(all, 2);
      for (int i = 0; i < all.Length - 1; i++)
      {
        if (!((int)all[i] > (int)all[i + 1]))
        {
          return false;
        }
      }
      return true;
    }


    [Builtin("fx<?")]
    public static object FxLess(params object[] all)
    {
      ExpectAtLeast(all, 2);
      for (int i = 0; i < all.Length - 1; i++)
      {
        if (!((int)all[i] < (int)all[i + 1]))
        {
          return false;
        }
      }
      return true;
    }

    [Builtin("fx>=?")]
    public static object FxGreaterOrEqual(params object[] all)
    {
      ExpectAtLeast(all, 2);
      for (int i = 0; i < all.Length - 1; i++)
      {
        if (!((int)all[i] >= (int)all[i + 1]))
        {
          return false;
        }
      }
      return true;
    }


    [Builtin("fx<=?")]
    public static object FxLessOrEqual(params object[] all)
    {
      ExpectAtLeast(all, 2);
      for (int i = 0; i < all.Length - 1; i++)
      {
        if (!((int)all[i] <= (int)all[i + 1]))
        {
          return false;
        }
      }
      return true;
    }

    [Builtin("fxzero?")]
    public static object FxIsZero(object a)
    {
      return (int)a == 0;
    }

    [Builtin("fxpositive?")]
    public static object FxIsPositive(object a)
    {
      return (int)a > 0;
    }

    [Builtin("fxnegative?")]
    public static object FxIsNegative(object a)
    {
      return (int)a < 0;
    }

    [Builtin("fxodd?")]
    public static object FxIsOdd(object a)
    {
      return (int)a % 2 == 1;
    }

    [Builtin("fxeven?")]
    public static object FxIsEven(object a)
    {
      return (int)a % 2 == 0;
    }

    [Builtin("fxmax")]
    public static object FxMax(params object[] args)
    {
      ExpectAtLeast(args, 1);
      int arglen = args.Length;
      switch (arglen)
      {
        case 1:
          return args[0];
        case 2:
          return Math.Max((int)args[0],(int)args[1]);
        default:
          object[] head = ArrayUtils.RemoveLast(args);
          return FxMax(FxMax(head), args[arglen - 1]);
      }
    }

    [Builtin("fxmin")]
    public static object FxMin(params object[] args)
    {
      ExpectAtLeast(args, 1);
      int arglen = args.Length;
      switch (arglen)
      {
        case 1:
          return args[0];
        case 2:
          return Math.Min((int)args[0], (int)args[1]);
        default:
          object[] head = ArrayUtils.RemoveLast(args);
          return FxMin(FxMin(head), args[arglen - 1]);
      }
    }


    [Builtin("fx+")]
    public static object FxAdd(object a, object b)
    {
      return (int)a + (int)b;
    }

    [Builtin("fx*")]
    public static object FxMultiply(object a, object b)
    {
      return (int)a * (int)b;
    }

    [Builtin("fx-")]
    public static object FxMinus(object a)
    {
      return -(int)a;
    }

    [Builtin("fx-")]
    public static object FxMinus(object a, object b)
    {
      return (int)a - (int)b;
    }


    [Builtin("fxdiv-and-mod")]
    public static object FxDivAndMod(object a, object b)
    {
      return Values(FxDiv(a,b), FxMod(a,b));
    }

    [Builtin("fxdiv")]
    public static object FxDiv(object a, object b)
    {
      return (int)a / (int)b;
    }


    [Builtin("fxmod")]
    public static object FxMod(object a, object b)
    {
      return (int)a % (int)b;
    }

    [Builtin("fxdiv0-and-mod0")]
    public static object FxDiv0AndMod0(object a, object b)
    {
      return Values(FxDiv0(a, b), FxMod0(a, b));
    }

    [Builtin("fxdiv0")]
    public static object FxDiv0(object a, object b)
    {
      return (int)a / (int)b;
    }


    [Builtin("fxmod0")]
    public static object FxMod0(object a, object b)
    {
      return (int)a % (int)b;
    }

    //(fx+/carry fx1 fx2 fx3)
    //(fx-/carry fx1 fx2 fx3)
    //(fx*/carry fx1 fx2 fx3)
    

    [Builtin("fxnot")]
    public static object FxNot(object a)
    {
      return ~(int)a;
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
          return args[0];
        case 2:
          return (int)args[0] & (int)args[1];
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
          return -1;
        case 1:
          return args[0];
        case 2:
          return (int)args[0] | (int)args[1];
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
          return -1;
        case 1:
          return args[0];
        case 2:
          return (int)args[0] ^ (int)args[1];
        default:
          object[] head = ArrayUtils.RemoveLast(args);
          return FxXor(FxXor(head), args[arglen - 1]);
      }
    }

    //(fxif fx1 fx2 fx3)
    //(fxcopy-bit fx1 fx2 fx3)
    //(fxbit-field fx1 fx2 fx3)
    //(fxcopy-bit-field fx1 fx2 fx3 fx4)
    //(fxarithmetic-shift-left fx1 fx2) 
    //(fxarithmetic-shift-right fx1 fx2) 
    //(fxrotate-bit-field fx1 fx2 fx3 fx4)
    //(fxreverse-bit-field fx1 fx2 fx3)

    //(fxbit-count fx)
    [Builtin("fxbit-count")]
    public static object FxBitCount(object ei)
    {
      int bi = (int)ei;

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
      int bi = (int)ei;

      if (bi <= 0)
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
      int bi = (int)ei;

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
      int bi = (int)ei;
      int ki = (int)k;

      if (ki < 0)
      {
        throw new SchemeException("fx-bit-set?", "k is negative", new string[] { k.ToString() });
      }

      if (bi == 0)
      {
        return false;
      }
      else
      {
        int count = 0;
        while (bi != 0)
        {
          if ((int)(bi & 1) == 1 && count == ki)
          {
            return true;
          }
          count++;
          bi >>= 1;
        }
        return false;
      }
    }

    //(fxarithmetic-shift fx1 fx2)
    [Builtin("fxarithmetic-shift")]
    public static object FxArithmeticShift(object ei, object k)
    {
      int bi = (int)ei;
      int ki = (int)k;

      if (ki == 0)
      {
        return ei;
      }
      if (ki < 0)
      {
        return bi >> ki;
      }
      else
      {
        return bi << ki;
      }
    }



  }
}
