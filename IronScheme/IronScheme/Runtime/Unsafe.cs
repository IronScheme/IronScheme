using System;
using System.Collections.Generic;
using System.Text;
using IronScheme.Compiler;
using Microsoft.Scripting.Ast;
using System.Reflection;
using System.Diagnostics;

namespace IronScheme.Runtime
{
  public static partial class BuiltinEmitters
  {
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
       

    internal class Unsafe
    {
      static bool Expect(Expression[] args, int count)
      {
        if (args.Length == count &&
          Array.TrueForAll(args, delegate(Expression e) { return e.Type == typeof(object) || e.Type == typeof(int); }))
        {
          return true;
        }
        else
        {
          Builtins.SyntaxError(false, string.Format("expected {0} arguments", count), args, false);
          return false;
        }
      }


      [InlineEmitter("$break")]
      public static Expression Break(Expression[] args)
      {
        if (Expect(args, 0))
        {
          return Ast.Call(typeof(Debugger).GetMethod("Break"));
        }
        return null;
      }

      [InlineEmitter("$try")]
      public static Expression Try(Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.Void(Ast.Try(Ast.Return(args[0])).Catch(typeof(OverflowException), Ast.Return(args[1])));
        }
        return null;
      }

      #region car + cdr

      static FieldInfo car = typeof(Cons).GetField("car");
      static FieldInfo cdr = typeof(Cons).GetField("cdr");

      [InlineEmitter("$car")]
      public static Expression Car(Expression[] values)
      {
        return ConsAccessor(values, car);
      }

      [InlineEmitter("$cdr")]
      public static Expression Cdr(Expression[] values)
      {
        return ConsAccessor(values, cdr);
      }

      static Expression ConsAccessor(Expression[] values, FieldInfo field)
      {
        if (values.Length != 1)
        {
          Builtins.SyntaxError(field.Name, "incorrect number of arguments", values, false);
        }
        Expression e = values[0];
        ConstantExpression c = e as ConstantExpression;
        if (c != null && c.Value == null)
        {
          Builtins.SyntaxError(field.Name, "unexpected constant expression", values, false);
        }

        if (e.Type != typeof(object) && e.Type != typeof(Cons))
        {
          Builtins.SyntaxError(field.Name, "unexpected type", values, false);
        }
        return Ast.ReadField(Ast.ConvertHelper(e, typeof(Cons)), field);
      }

      #endregion

      #region vector-ref + vector-set!

      //[InlineEmitter("$vector-length")]
      //public static Expression VectorLength(Expression[] values)
      //{
      //  if (values.Length == 1)
      //  {
      //    return Ast.ArrayL(Ast.ConvertHelper(values[0], typeof(object[])));
      //  }
      //  Builtins.SyntaxError("$vector-length", "expected 1 argument", values, false);
      //  return null;
      //}

      [InlineEmitter("$vector-ref")]
      public static Expression VectorRef(Expression[] values)
      {
        if (values.Length == 2)
        {
          return Ast.ArrayIndex(Ast.ConvertHelper(values[0], typeof(object[])), Ast.ConvertHelper(values[1], typeof(int)));
        }
        Builtins.SyntaxError("$vector-ref", "expected 2 arguments", values, false);
        return null;
      }

      [InlineEmitter("$vector-set!")]
      public static Expression VectorSet(Expression[] values)
      {
        if (values.Length == 3)
        {
          return Ast.AssignArrayIndex(Ast.ConvertHelper(values[0], typeof(object[])), Ast.ConvertHelper(values[1], typeof(int)), values[2]);
        }
        Builtins.SyntaxError("$vector-set!", "expected 3 arguments", values, false);
        return null;
      }

      #endregion

      #region bytevector-ref + bytevector-set!

      [InlineEmitter("$bytevector-ref")]
      public static Expression ByteVectorRef(Expression[] values)
      {
        if (values.Length == 2)
        {
          return Ast.ArrayIndex(Ast.ConvertHelper(values[0], typeof(byte[])), Ast.ConvertHelper(values[1], typeof(int)));
        }
        Builtins.SyntaxError("$bytevector-ref", "expected 2 arguments", values, false);
        return null;
      }

      [InlineEmitter("$bytevector-set!")]
      public static Expression ByteVectorSet(Expression[] values)
      {
        if (values.Length == 3)
        {
          return Ast.AssignArrayIndex(Ast.ConvertHelper(values[0], typeof(byte[])), Ast.ConvertHelper(values[1], typeof(int)), Ast.ConvertHelper(values[2], typeof(byte)));
        }
        Builtins.SyntaxError("$bytevector-set!", "expected 3 arguments", values, false);
        return null;
      }

      #endregion

      #region fixnums

      [InlineEmitter("$fx=?")]
      public static Expression FxEquals(params Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.Equal(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fx+")]
      public static Expression FxAdd(params Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.AddChecked(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fx*")]
      public static Expression FxMultiply(params Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.MultiplyChecked(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fx-")]
      public static Expression FxMinus(params Expression[] args)
      {
        if (args.Length == 1)
        {
          return Ast.SubtractChecked(Ast.Zero(), UnwrapAndCast<int>(args[0]));
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

      [InlineEmitter("$fx<?")]
      public static Expression FxIsLessThan(params Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.LessThan(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fx<=?")]
      public static Expression FxIsLessThanOrEqual(params Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.LessThanEquals(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fx>?")]
      public static Expression FxIsGreater(params Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.GreaterThan(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fx>=?")]
      public static Expression FxIsGreaterOrEqual(params Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.GreaterThanEquals(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fxand")]
      public static Expression FxAnd(params Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.And(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fxior")]
      public static Expression FxIor(params Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.Or(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fxxor")]
      public static Expression FxXor(params Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.ExclusiveOr(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fxnot")]
      public static Expression FxNot(params Expression[] args)
      {
        if (Expect(args, 1))
        {
          return Ast.Not(UnwrapAndCast<int>(args[0]));
        }
        return null;
      }

      [InlineEmitter("$fxdiv0")]
      public static Expression FxDiv0(params Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.Divide(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fxmod0")]
      public static Expression FxMod0(params Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.Modulo(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fxarithmetic-shift-left")]
      public static Expression FxArithmeticShiftLeft(params Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.LeftShift(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fxarithmetic-shift-right")]
      public static Expression FxArithmeticShiftRight(params Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.RightShift(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }


      #endregion

      #region flonums

      static bool Expect2(Expression[] args, int count)
      {
        if (args.Length == count &&
          Array.TrueForAll(args, delegate(Expression e) { return e.Type == typeof(object) || e.Type == typeof(double); }))
        {
          return true;
        }
        else
        {
          Builtins.SyntaxError(false, string.Format("expected {0} arguments", count), args, false);
          return false;
        }
      }

      [InlineEmitter("$fl=?")]
      public static Expression FlEquals(params Expression[] args)
      {
        if (Expect2(args, 2))
        {
          return Ast.Equal(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fl+")]
      public static Expression FlAdd(params Expression[] args)
      {
        if (Expect2(args, 2))
        {
          return Ast.Add(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fl*")]
      public static Expression FlMultiply(params Expression[] args)
      {
        if (Expect2(args, 2))
        {
          return Ast.Multiply(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fl-")]
      public static Expression FlMinus(params Expression[] args)
      {
        if (args.Length == 1)
        {
          return Ast.Negate(UnwrapAndCast<double>(args[0]));
        }
        else
        {
          if (Expect2(args, 2))
          {
            return Ast.Subtract(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
          }
          return null;
        }
      }

      [InlineEmitter("$fl<?")]
      public static Expression FlIsLessThan(params Expression[] args)
      {
        if (Expect2(args, 2))
        {
          return Ast.LessThan(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fl<=?")]
      public static Expression FlIsLessThanOrEqual(params Expression[] args)
      {
        if (Expect2(args, 2))
        {
          return Ast.LessThanEquals(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fl>?")]
      public static Expression FlIsGreater(params Expression[] args)
      {
        if (Expect2(args, 2))
        {
          return Ast.GreaterThan(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fl>=?")]
      public static Expression FlIsGreaterOrEqual(params Expression[] args)
      {
        if (Expect2(args, 2))
        {
          return Ast.GreaterThanEquals(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fl/")]
      public static Expression FlQuotient(params Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.Divide(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        }
        return null;
      }



      #endregion

    }

    internal class Unchecked
    {
      static bool Expect(Expression[] args, int count)
      {
        if (args.Length == count &&
          Array.TrueForAll(args, delegate(Expression e) { return e.Type == typeof(object) || e.Type == typeof(int); }))
        {
          return true;
        }
        else
        {
          Builtins.SyntaxError(false, string.Format("expected {0} arguments", count), args, false);
          return false;
        }
      }

      #region fixnums
      
      [InlineEmitter("$$fx+")]
      public static Expression FxAdd(params Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.Add(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$$fx*")]
      public static Expression FxMultiply(params Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.Multiply(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$$fx-")]
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
      #endregion
    }
  }
}
