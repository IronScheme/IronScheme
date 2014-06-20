#region License
/* Copyright (c) 2007-2014 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System.Reflection;
using IronScheme.Compiler;
using Microsoft.Scripting.Ast;
using System;

namespace IronScheme.Runtime
{
  static partial class BuiltinEmitters
  {
    [InlineEmitter("null?")]
    public static Expression IsNull(Expression[] values)
    {
      if (values.Length != 1)
      {
        return null;
      }
      return Ast.Equal(values[0], Ast.Null());
    }
    
    [InlineEmitter("pair?")]
    public static Expression IsPair(Expression[] values)
    {
      if (values.Length != 1)
      {
        return null;
      }
      return Ast.TypeIs(values[0], typeof(Cons));
    }

    static readonly FieldInfo car = typeof(Cons).GetField("car");
    static readonly FieldInfo cdr = typeof(Cons).GetField("cdr");

    //[InlineEmitter("car", Optimization=OptimizationLevel.Safe)]
    public static Expression Car(Expression[] values)
    {
      return ConsAccessor(values, car);
    }

    //[InlineEmitter("cdr", Optimization = OptimizationLevel.Safe)]
    public static Expression Cdr(Expression[] values)
    {
      return ConsAccessor(values, cdr);
    }

    static Expression ConsAccessor(Expression[] values, FieldInfo field)
    {
      if (values.Length != 1)
      {
        return null;
      }
      Expression e = values[0];
      ConstantExpression c = e as ConstantExpression;
      if (c != null && c.Value == null)
      {
        return null;
      }

      if (e.Type != typeof(object) && e.Type != typeof(Cons))
      {
        return null;
      }
      return Ast.ReadField(Ast.ConvertHelper(e, typeof(Cons)), field);
    }

    static ConstructorInfo cons = typeof(Cons).GetConstructor(new Type[] { typeof(object), typeof(object) });

    [InlineEmitter("cons")]
    public static Expression Cons(Expression[] values)
    {
      if (values.Length == 2)
      {
        return Ast.New(cons, values[0], values[1]);  
      }
      return null;
    }
  }

  public partial class Builtins
  {
    [Builtin("pair?")]
    public static object IsPair(object arg1)
    {
      return GetBool(arg1 is Cons);
    }

    [Builtin("null?")]
    public static object IsNull(object arg1)
    {
      return GetBool(arg1 == null);
    }

    [Builtin("list-prim", UsedInternallyByCompiler = true)]
    public static object List(params object[] args)
    {
      return Runtime.Cons.FromArray(args);
    }

    [Builtin("list-prim", UsedInternallyByCompiler = true)]
    public static object List()
    {
      return null;
    }

    [Builtin("list-prim", UsedInternallyByCompiler = true)]
    public static object List(object arg1)
    {
      return new Cons(arg1);
    }

    [Builtin("list-prim", UsedInternallyByCompiler = true)]
    public static object List(object arg1, object arg2)
    {
      return new Cons(arg1, new Cons(arg2));
    }

    [Builtin("list-prim", UsedInternallyByCompiler = true)]
    public static object List(object arg1, object arg2, object arg3)
    {
      return new Cons(arg1, new Cons(arg2, new Cons(arg3)));
    }

    [Builtin("list-prim", UsedInternallyByCompiler = true)]
    public static object List(object arg1, object arg2, object arg3, object arg4)
    {
      return new Cons(arg1, new Cons(arg2, new Cons(arg3, new Cons(arg4))));
    }

    [Builtin("list-prim", UsedInternallyByCompiler = true)]
    public static object List(object arg1, object arg2, object arg3, object arg4, object arg5)
    {
      return new Cons(arg1, new Cons(arg2, new Cons(arg3, new Cons(arg4, new Cons(arg5)))));
    }

    // overload, no export, bad me!
    public static Cons Cons(object car)
    {
      return new Cons(car);
    }

    [Builtin]
    public static object Cons(object car, object cdr)
    {
      return new Cons(car, cdr);
    }

    internal static object First(object args)
    {
      return Car(args);
    }

    internal static object Second(object args)
    {
      return Car(Cdr(args));
    }

    internal static object Third(object args)
    {
      return Car(Cdr(Cdr(args)));
    }

    internal static Cons LastPair(object args)
    {
      Cons c = Requires<Runtime.Cons>(args);
      while (c.cdr is Cons)
      {
        c = c.cdr as Cons;
      }
      return c;
    }
    
    [Builtin]
    public static object Car(object args)
    {
      Cons c = args as Cons;
      if (c == null)
      {
        AssertionViolation("car", "not a pair", args);        
      }
      return c.car;
    }

    [Builtin]
    public static object Cdr(object args)
    {
      Cons c = args as Cons;
      if (c == null)
      {
        AssertionViolation("cdr", "not a pair", args);
      }
      return c.cdr;
    }

    public static object Append(object arg1, object arg2)
    {
      if (arg1 == null)
      {
        return arg2;
      }
      if (arg2 == null)
      {
        return arg1;
      }
      Cons c = arg1 as Cons;
      do
      {
        if (c.cdr == null)
        {
          break;
        }
        c = c.cdr as Cons;
      }
      while (true);

      c.cdr = arg2;

      return arg1;
    }

    [Builtin("reverse!")]
    public static object NReverse(object lst)
    {
      Cons list = lst as Cons;

      if (list == null)
      {
        return null;
      }

      Cons prev = null, next = null;

      while (true)
      {
        next = list.cdr as Cons;
        list.cdr = prev;

        if (next == null)
        {
          return list;
        }

        prev = list;
        list = next;
      }
    }

    internal static Cons ToImproper(Cons c)
    {
      Cons i = c;
      Cons j = null;

      while (i.cdr != null)
      {
        j = i;
        i = i.cdr as Cons;
        if (i == null)
        {
          return c; // improper already
        }
      }

      j.cdr = i.car;
      return c;
    }

  }
}
