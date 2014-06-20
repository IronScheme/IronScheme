#region License
/* Copyright (c) 2007-2014 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Diagnostics;
using System.Reflection;
using IronScheme.Compiler;
using Microsoft.Scripting.Ast;

namespace IronScheme.Runtime
{
  static partial class BuiltinEmitters
  {
    static void UnsafeSyntaxError(object who, string msg, object form)
    {
      Builtins.SyntaxError(who, msg, form, Builtins.FALSE);
    }

    static Expression UnwrapAndCast<T>(Expression e)
    {
      e = Unwrap(e);
      if (e.Type == typeof(object) || e.Type == typeof(T) || (typeof(T) == typeof(int) && e.Type == typeof(byte)))
      {
        return Ast.ConvertHelper(Unwrap(e), typeof(T));
      }
      UnsafeSyntaxError("UnwrapAndCast", "Expected fixnum, but got " + e.Type.Name + ".", e);
      return null;
    }

    static bool Expect(Expression[] args, int count)
    {
      if (args.Length == count)
      {
        return true;
      }
      else
      {
        UnsafeSyntaxError(Builtins.FALSE, string.Format("expected {0} arguments", count), args);
        return false;
      }
    }

    static bool Expect<T>(Expression[] args, int count)
    {
      if (args.Length == count &&
        Array.TrueForAll(args, delegate(Expression e) { var ex = Unwrap(e); return e.Type == typeof(object) || ex.Type == typeof(T); }))
      {
        return true;
      }
      else
      {
        UnsafeSyntaxError(Builtins.FALSE, string.Format("expected {0} arguments", count), args);
        return false;
      }
    }
       

    internal class Unsafe
    {
      [InlineEmitter("$and?")]
      public static Expression BooleanAnd(Expression[] args)
      {
        int len = args.Length;
        switch (len)
        {
          case 0:
            return Ast.Constant(true);
          default:
            var rargs = new Expression[len - 1];
            Array.Copy(args, 1, rargs, 0, rargs.Length);
            return Ast.AndAlso(Ast.ConvertHelper(args[0], typeof(bool)), BooleanAnd(rargs));
        }
      }

      [InlineEmitter("$or?")]
      public static Expression BooleanOr(Expression[] args)
      {
        int len = args.Length;
        switch (len)
        {
          case 0:
            return Ast.Constant(false);
          default:
            var rargs = new Expression[len - 1];
            Array.Copy(args, 1, rargs, 0, rargs.Length);
            return Ast.OrElse(Ast.ConvertHelper(args[0], typeof(bool)), BooleanOr(rargs));
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

      [InlineEmitter("$throw")]
      public static Expression Throw(Expression[] args)
      {
        if (Expect(args, 1))
        {
          return Ast.Void(Ast.Throw(args[0]));
        }
        return null;
      }


      [InlineEmitter("$try/finally")]
      public static Expression TryFinally(Expression[] args)
      {
        if (Expect(args, 2))
        {
          return Ast.Void(Ast.Try(Ast.Return(args[0])).Finally(Ast.Statement(args[1])));
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
          UnsafeSyntaxError(field.Name, "incorrect number of arguments", values);
        }
        Expression e = values[0];
        ConstantExpression c = e as ConstantExpression;
        if (c != null && c.Value == null)
        {
          UnsafeSyntaxError(field.Name, "unexpected constant expression", values);
        }

        if (e.Type != typeof(object) && e.Type != typeof(Cons))
        {
          UnsafeSyntaxError(field.Name, "unexpected type", values);
        }
        return Ast.ReadField(Ast.ConvertHelper(e, typeof(Cons)), field);
      }

      #endregion

      #region vector-ref + vector-set!

      [InlineEmitter("$vector-ref")]
      public static Expression VectorRef(Expression[] values)
      {
        if (values.Length == 2)
        {
          if (!typeof(Array).IsAssignableFrom(values[0].Type))
          {
            values[0] = Ast.ConvertHelper(values[0], typeof(object[]));
          }
          return Ast.ArrayIndex(values[0], Ast.ConvertHelper(values[1], typeof(int)));
        }
        UnsafeSyntaxError("$vector-ref", "expected 2 arguments", values);
        return null;
      }

      [InlineEmitter("$vector-set!")]
      public static Expression VectorSet(Expression[] values)
      {
        if (values.Length == 3)
        {
          if (!typeof(Array).IsAssignableFrom(values[0].Type))
          {
            values[0] = Ast.ConvertHelper(values[0], typeof(object[]));
          }
          return Ast.AssignArrayIndex(values[0], Ast.ConvertHelper(values[1], typeof(int)), values[2]);
        }
        UnsafeSyntaxError("$vector-set!", "expected 3 arguments", values);
        return null;
      }

      #endregion

      #region bytevector-ref + bytevector-set!

      [InlineEmitter("$bytevector-ref")]
      public static Expression ByteVectorRef(Expression[] values)
      {
        if (values.Length == 2)
        {
          return Ast.Convert(Ast.ArrayIndex(Ast.ConvertHelper(values[0], typeof(byte[])), Ast.ConvertHelper(values[1], typeof(int))), typeof(int));
        }
        UnsafeSyntaxError("$bytevector-ref", "expected 2 arguments", values);
        return null;
      }

      [InlineEmitter("$bytevector-set!")]
      public static Expression ByteVectorSet(Expression[] values)
      {
        if (values.Length == 3)
        {
          return Ast.AssignArrayIndex(Ast.ConvertHelper(values[0], typeof(byte[])), Ast.ConvertHelper(values[1], typeof(int)), Ast.ConvertHelper(values[2], typeof(byte)));
        }
        UnsafeSyntaxError("$bytevector-set!", "expected 3 arguments", values);
        return null;
      }

      #endregion

      #region fixnums

      [InlineEmitter("$fx=?")]
      public static Expression FxEquals(params Expression[] args)
      {
        if (Expect<int>(args, 2))
        {
          return Ast.Equal(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fx+")]
      public static Expression FxAdd(params Expression[] args)
      {
        if (Expect<int>(args, 2))
        {
          return Ast.Add(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fx*")]
      public static Expression FxMultiply(params Expression[] args)
      {
        if (Expect<int>(args, 2))
        {
          return Ast.Multiply(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fx-")]
      public static Expression FxMinus(params Expression[] args)
      {
        if (args.Length == 1)
        {
          return Ast.Subtract(Ast.Zero(), UnwrapAndCast<int>(args[0]));
        }
        else
        {
          if (Expect<int>(args, 2))
          {
            return Ast.Subtract(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
          }
          return null;
        }
      }

      [InlineEmitter("$fx<?")]
      public static Expression FxIsLessThan(params Expression[] args)
      {
        if (Expect<int>(args, 2))
        {
          return Ast.LessThan(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fx<=?")]
      public static Expression FxIsLessThanOrEqual(params Expression[] args)
      {
        if (Expect<int>(args, 2))
        {
          return Ast.LessThanEquals(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fx>?")]
      public static Expression FxIsGreater(params Expression[] args)
      {
        if (Expect<int>(args, 2))
        {
          return Ast.GreaterThan(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fx>=?")]
      public static Expression FxIsGreaterOrEqual(params Expression[] args)
      {
        if (Expect<int>(args, 2))
        {
          return Ast.GreaterThanEquals(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fxand")]
      public static Expression FxAnd(params Expression[] args)
      {
        if (Expect<int>(args, 2))
        {
          return Ast.And(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fxior")]
      public static Expression FxIor(params Expression[] args)
      {
        if (Expect<int>(args, 2))
        {
          return Ast.Or(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fxxor")]
      public static Expression FxXor(params Expression[] args)
      {
        if (Expect<int>(args, 2))
        {
          return Ast.ExclusiveOr(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fxnot")]
      public static Expression FxNot(params Expression[] args)
      {
        if (Expect<int>(args, 1))
        {
          return Ast.Not(UnwrapAndCast<int>(args[0]));
        }
        return null;
      }

      [InlineEmitter("$fxdiv0")]
      [InlineEmitter("$fxdiv")]
      public static Expression FxDiv(params Expression[] args)
      {
        if (Expect<int>(args, 2))
        {
          return Ast.Divide(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fxmod0")]
      [InlineEmitter("$fxmod")]
      public static Expression FxMod(params Expression[] args)
      {
        if (Expect<int>(args, 2))
        {
          return Ast.Modulo(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fxarithmetic-shift-left")]
      public static Expression FxArithmeticShiftLeft(params Expression[] args)
      {
        if (Expect<int>(args, 2))
        {
          return Ast.LeftShift(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fxarithmetic-shift-right")]
      public static Expression FxArithmeticShiftRight(params Expression[] args)
      {
        if (Expect<int>(args, 2))
        {
          return Ast.RightShift(UnwrapAndCast<int>(args[0]), UnwrapAndCast<int>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fxzero?")]
      public static Expression FxZero(params Expression[] args)
      {
        if (Expect<int>(args, 1))
        {
          return Ast.Equal(UnwrapAndCast<int>(args[0]), Ast.Constant(0));
        }
        return null;
      }

      [InlineEmitter("$fxnegative?")]
      public static Expression FxNegative(params Expression[] args)
      {
        if (Expect<int>(args, 1))
        {
          return Ast.LessThan(UnwrapAndCast<int>(args[0]), Ast.Constant(0));
        }
        return null;
      }

      [InlineEmitter("$fxpositive?")]
      public static Expression FxPositive(params Expression[] args)
      {
        if (Expect<int>(args, 1))
        {
          return Ast.GreaterThan(UnwrapAndCast<int>(args[0]), Ast.Constant(0));
        }
        return null;
      }

      [InlineEmitter("$fxeven?")]
      public static Expression FxEven(params Expression[] args)
      {
        if (Expect<int>(args, 1))
        {
          return Ast.Equal(Ast.Add(UnwrapAndCast<int>(args[0]), Ast.Constant(1)), Ast.Constant(0));
        }
        return null;
      }

      [InlineEmitter("$fxodd?")]
      public static Expression FxOdd(params Expression[] args)
      {
        if (Expect<int>(args, 1))
        {
          return Ast.Equal(Ast.Add(UnwrapAndCast<int>(args[0]), Ast.Constant(1)), Ast.Constant(1));
        }
        return null;
      }

      #endregion

      #region flonums

      [InlineEmitter("$fl=?")]
      public static Expression FlEquals(params Expression[] args)
      {
        if (Expect<double>(args, 2))
        {
          return Ast.Equal(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fl+")]
      public static Expression FlAdd(params Expression[] args)
      {
        if (Expect<double>(args, 2))
        {
          return Ast.Add(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fl*")]
      public static Expression FlMultiply(params Expression[] args)
      {
        if (Expect<double>(args, 2))
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
          if (Expect<double>(args, 2))
          {
            return Ast.Subtract(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
          }
          return null;
        }
      }

      [InlineEmitter("$fl<?")]
      public static Expression FlIsLessThan(params Expression[] args)
      {
        if (Expect<double>(args, 2))
        {
          return Ast.LessThan(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fl<=?")]
      public static Expression FlIsLessThanOrEqual(params Expression[] args)
      {
        if (Expect<double>(args, 2))
        {
          return Ast.LessThanEquals(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fl>?")]
      public static Expression FlIsGreater(params Expression[] args)
      {
        if (Expect<double>(args, 2))
        {
          return Ast.GreaterThan(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fl>=?")]
      public static Expression FlIsGreaterOrEqual(params Expression[] args)
      {
        if (Expect<double>(args, 2))
        {
          return Ast.GreaterThanEquals(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        }
        return null;
      }

      [InlineEmitter("$fl/")]
      public static Expression FlQuotient(params Expression[] args)
      {
        if (Expect<double>(args, 2))
        {
          return Ast.Divide(UnwrapAndCast<double>(args[0]), UnwrapAndCast<double>(args[1]));
        }
        return null;
      }



      #endregion

    }
  }
}
