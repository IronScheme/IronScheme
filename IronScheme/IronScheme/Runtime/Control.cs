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
using Microsoft.Scripting.Math;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Collections;
using Microsoft.Scripting;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Ast;
using System.Diagnostics;
using IronScheme.Compiler;
using System.Threading;

namespace IronScheme.Runtime
{
  public static partial class BuiltinEmitters
  {
    static MethodInfo ICallable_Call = typeof(ICallable).GetMethod("Call", new Type[] { typeof(object[]) });
    static MethodInfo ListToVector = typeof(Builtins).GetMethod("ListToVector");
    static MethodInfo List = typeof(Builtins).GetMethod("List", new Type[] { typeof(object[]) });
    static MethodInfo Append = typeof(Builtins).GetMethod("Append", new Type[] { typeof(object), typeof(object) });
    

    [InlineEmitter("apply")]
    public static Expression Apply(Expression[] args)
    {
      Expression c = Ast.ConvertHelper(args[0], typeof(ICallable));
      if (args.Length == 2)
      {
        return Ast.ComplexCallHelper(c, ICallable_Call, Ast.Call(ListToVector, args[1]));
      }
      else if (args.Length > 2)
      {
        Expression head = Ast.ComplexCallHelper(List, ArrayUtils.RemoveFirst(ArrayUtils.RemoveLast(args)));
        Expression cargs = Ast.ComplexCallHelper(Append, head, args[args.Length - 1]);
        
        return Ast.ComplexCallHelper(c, ICallable_Call, Ast.Call(ListToVector, cargs));
      }
      else
      {
        return null;
      }
    }

    [InlineEmitter("void")]
    public static Expression Void(Expression[] values)
    {
      return Ast.ReadField(null, Compiler.Generator.Unspecified);
    }
  }

  public partial class Builtins
  {
    [Builtin]
    public static object Values(params object[] values)
    {
      if (values.Length == 1)
      {
        return values[0];
      }
      return new MultipleValues(values);
    }

    [Builtin("dynamic-wind")]
    public static object DynamicWind(object infunc, object bodyfunc, object outfunc)
    {
      ICallable inf = RequiresNotNull<ICallable>(infunc);
      ICallable bodyf = RequiresNotNull<ICallable>(bodyfunc);
      ICallable outf = RequiresNotNull<ICallable>(outfunc);

#if CPS
      OptimizedBuiltins.Call(inf);

      try
      {
        return OptimizedBuiltins.Call(bodyf);
      }
      finally
      {
        OptimizedBuiltins.Call(outf);
      }
#else
      inf.Call();

      try
      {
        return bodyf.Call();
      }
      finally
      {
        outf.Call();
      }
#endif
    }

#if CPS
    [Builtin("call-with-current-continuation"), Builtin("call/cc")]
    public static object CallWithCurrentContinuation(object k, object fc1)
    {
      ICallable fc = RequiresNotNull<ICallable>(fc1);
      ICallable e = RequiresNotNull<ICallable>(k);

      CallTarget2 esc = delegate(object ignore, object arg)
      {
        return e.Call(arg);
      };

      if (fc is BuiltinMethod)
      {
        return e.Call(fc.Call(Closure.Make(null, esc)));
      }
      else
      {
        return fc.Call(k, Closure.Make(null, esc));
      }
    }

#else
    internal class Continuation : Exception
    {
      object value;

      public object Value
      {
        get { return this.value; }
        set { this.value = value; }
      } 
    }

    static CallTargetN MakeContinuation(Continuation cc)
    {
      CallTargetN ct = delegate(object[] value)
      {
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

    //this needs to be better controlled, and perhaps use weak references
    internal static Stack<Continuation> contstack = new Stack<Continuation>();
    
    [Builtin("call-with-current-continuation"), Builtin("call/cc")]
    public static object CallWithCurrentContinuation(object fc1)
    {
      ICallable fc = RequiresNotNull<ICallable>(fc1);
      Continuation ccc = new Continuation();
      contstack.Push(ccc);
      try
      {
        CallTargetN exitproc = MakeContinuation(ccc);
        ICallable fce = Closure.Make(cc, exitproc);
        object res = fc.Call(fce);
        return res;
      }
      catch (Continuation c)
      {
        if (ccc == c)
        {
          contstack.Pop();
          return c.Value;
        }
        else
        {
          throw;
        }
      }
      finally
      {
        ;
      }
    }
#endif

    [Builtin("procedure?")]
    public static object IsProcedure(object obj)
    {
      return GetBool(obj is ICallable); 
    }


    //procedure:  (apply proc arg1 ... args) 
    //Proc must be a procedure and args must be a list. Calls proc with the elements of the list (append (list arg1 ...) args) as the actual arguments.
    
    [Builtin]
    public static object Apply(object fn, params object[] args)
    {
      if (args == null)
      {
        return Apply(fn, (object)null);
      }
      object[] head = ArrayUtils.RemoveLast(args);
      object last = args.Length > 0 ? args[args.Length - 1] : null;

      return Apply(fn, Append(List(head), last));
    }


    [Builtin]
    public static object Apply(object fn, object list)
    {
      Cons args = Requires<Runtime.Cons>(list);
      ICallable c = RequiresNotNull<ICallable>(fn);

      if (args == null)
      {
#if CPS
        if (!(fn is BuiltinMethod))
        {
          return c.Call(SymbolValue(SymbolTable.StringToId("values")));
        }
#endif
        return c.Call();
      }
      List<object> targs = new List<object>();
      
      while (args != null)
      {
        targs.Add(args.car);
        args = args.cdr as Cons;
      }

#if CPS
      if (!(fn is BuiltinMethod))
      {
        targs.Insert(0, SymbolValue(SymbolTable.StringToId("values")));
      }
#endif

      return c.Call(targs.ToArray());
    }
  }
}
