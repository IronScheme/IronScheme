#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using IronScheme.Compiler;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;

namespace IronScheme.Runtime
{
  public static partial class BuiltinEmitters
  {
    readonly static MethodInfo ICallable_Call = typeof(Callable).GetMethod("Call", new Type[] { typeof(object[]) });
    readonly static MethodInfo ListToVector = typeof(Builtins).GetMethod("ListToVector", new [] { typeof(Cons) });
    readonly static ConstructorInfo Cons_ctr = typeof(Cons).GetConstructor(new[] { typeof(object), typeof(object) });

    [InlineEmitter("apply")]
    public static Expression Apply(Expression[] args)
    {
      Expression c = Ast.ConvertHelper(args[0], typeof(Callable));
      if (args.Length > 1)
      {
        Expression arg = Ast.ConvertHelper(args[args.Length - 1], typeof(Cons));
        for (int i = args.Length - 2; i > 0; i--)
        {
          arg = MakeCons(args[i], arg);
        }
        
        return Ast.ComplexCallHelper(c, ICallable_Call, Ast.Call(ListToVector, arg));
      }
      else
      {
        return null;
      }
    }

    static Expression MakeCons(Expression car, Expression cdr)
    {
      return Ast.New(Cons_ctr, car , cdr);
    }

    [InlineEmitter("void")]
    public static Expression Void(Expression[] values)
    {
      return Ast.ReadField(null, Compiler.Generator.Unspecified);
    }
  }

  public partial class Builtins
  {
#if !CPS
    [Builtin]
#endif
    public static object Values(params object[] values)
    {
      if (values.Length == 1)
      {
        return values[0];
      }
      return new MultipleValues(values);
    }

#if CPS

#error CPS mode is currently broken, use revision 23730 or earlier

#else

    [Builtin]
    public static object Values()
    {
      return new MultipleValues();
    }

    [Builtin]
    public static object Values(object arg1)
    {
      return arg1;
    }

    [Builtin]
    public static object Values(object arg1, object arg2)
    {
      return new MultipleValues(arg1, arg2);
    }

    [Builtin]
    public static object Values(object arg1, object arg2, object arg3)
    {
      return new MultipleValues(arg1, arg2, arg3);
    }

    [Builtin]
    public static object Values(object arg1, object arg2, object arg3, object arg4)
    {
      return new MultipleValues(arg1, arg2, arg3, arg4);
    }

    internal class Continuation : Exception
    {
      public object Value {get; internal set;}
      internal StackTrace Stack { get; set; }
    }

    internal static readonly bool IsMono = Type.GetType("Mono.Runtime", false) != null;

    static CallTargetN MakeContinuation(Continuation cc)
    {
      CallTargetN ct = delegate(object[] value)
      {
        if (!CheckStack(cc)) 
        {
          return AssertionViolation("call/cc", "not supported, continuation called outside dynamic extent");
        }
        if (value.Length == 0)
        {
          cc.Value = Unspecified;
          throw cc;
        }
        if (value.Length == 1)
        {
          cc.Value = value[0];
          if (cc.Value is MultipleValues)
          {
            return AssertionViolation("call/cc", "cannot return multiple values");
          }
          throw cc;
        }
        else
        {
          cc.Value = Values(value);
          throw cc;
        }
      };

      return ct;
    }

    static bool CheckStack(Continuation cc)
    {
      var st = new StackTrace();
      var c1 = cc.Stack.GetFrames();
      var f1 = st.GetFrames();

      if (IsMono)
      {
        // mono: for some reason the one stack is reversed... but not all the time... FFFFFUUUUUU!! 8/
        if (c1[0].GetMethod().Name != "Main")
        {
          Array.Reverse(c1);
        }
        if (f1[0].GetMethod().Name != "Main")
        {
          Array.Reverse(f1);
        }
      }
      else
      {
        Array.Reverse(c1);
        Array.Reverse(f1);
      }

      for (int i = 0; i < c1.Length; i++)
      {
        if (c1[i].GetMethod() != f1[i].GetMethod())
        {
          return false;
        }
      }
      return true;
    }
    
    [Builtin("call-with-current-continuation"), Builtin("call/cc")]
    public static object CallWithCurrentContinuation(object fc1)
    {
      Callable fc = RequiresNotNull<Callable>(fc1);
      Continuation ccc = new Continuation { Stack = new StackTrace() };

      try
      {
        CallTargetN exitproc = MakeContinuation(ccc);
        Callable fce = Closure.Create(cc, exitproc) as Callable;
        object res = fc.Call(fce);
        return res;
      }
      catch (Continuation c)
      {
        if (ccc == c)
        {
          return c.Value;
        }
        else
        {
          throw;
        }
      }
    }
#endif


#if CPS

    public static object LetrecIdentity(object var)
    {
      bool init = false;
      CallTarget1 id = delegate(object V)
      {
        if (init)
        {
          AssertionViolation("letrec", "initialization continuation invoked more than once", UnGenSym(var));
        }
        init = true;
        return V;
      };

      return Closure.Make(null, id);
    }


    public static object LetrecStarIdentity(object var)
    {
      bool init = false;
      CallTarget1 id = delegate(object V)
      {
        if (init)
        {
          AssertionViolation("letrec*-identity", "initialization continuation invoked more than once", UnGenSym(var));
        }
        init = true;
        return V;
      };

      return Closure.Make(null, id);
    }

    public static object LibraryLetrecIdentity(object var)
    {
      bool init = false;
      CallTarget1 id = delegate(object V)
      {
        if (init)
        {
          AssertionViolation("library-letrec*-identity", "initialization continuation invoked more than once", UnGenSym(var));
        }
        init = true;
        return V;
      };

      return Closure.Make(null, id);
    }

#else
    //procedure:  (apply proc arg1 ... args) 
    //Proc must be a procedure and args must be a list. Calls proc with the elements of the list (append (list arg1 ...) args) as the actual arguments.
    [Builtin]
    public static object Apply(object fn, params object[] args)
    {
      if (args == null)
      {
        return Apply(fn, (object)null);
      }
      if (args.Length == 0)
      {
        return AssertionViolation("apply", "Expected at least 2 args", fn);
      }
      object[] head = ArrayUtils.RemoveLast(args);
      object last = args.Length > 0 ? args[args.Length - 1] : null;

      return Apply(fn, Append(Runtime.Cons.FromArray(head), last));
    }


    [Builtin]
    public static object Apply(object fn, object list)
    {
      Cons args = Requires<Runtime.Cons>(list);
      Callable c = RequiresNotNull<Callable>(fn);

      if (args == null)
      {
        return c.Call();
      }
      List<object> targs = new List<object>();
      
      while (args != null)
      {
        targs.Add(args.car);
        args = args.cdr as Cons;
      }

      return c.Call(targs.ToArray());
    }
#endif
  }
}
