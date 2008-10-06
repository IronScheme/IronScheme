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

      inf.Call();

      try
      {
        return bodyf.Call();
      }
      finally
      {
        outf.Call();
      }
    }


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

    delegate void Thunk();

    readonly static Stack<object> executionstack = new Stack<object>();

    static void PExecute(IList<Thunk> thunks)
    {
      if (executionstack.Count == 0)
      {
        executionstack.Push(thunks);

        List<IAsyncResult> results = new List<IAsyncResult>();

        foreach (Thunk t in thunks)
        {
          results.Add(t.BeginInvoke(null, null));
        }

        for (int i = 0; i < thunks.Count; i++)
        {
          thunks[i].EndInvoke(results[i]);
        }

        executionstack.Pop();
      }
      else
      {
        foreach (Thunk t in thunks)
        {
          t();
        }
      }
    }

    [Builtin("pmap")]
    public static object PMap(object fn, object lst)
    {
      Cons list = Requires<Runtime.Cons>(lst);
      ICallable f = RequiresNotNull<ICallable>(fn);

      object[] results = new object[(int)Length(lst)];

      List<Thunk> thunks = new List<Thunk>();

      int i = 0;

      while (list != null)
      {
        int index = i;
        object arg = list.car;
        thunks.Add(
         delegate
         {
           results[index] = f.Call(arg);
         });
        
        list = list.cdr as Cons;
        i++;
      }
      
      PExecute(thunks);

      return Runtime.Cons.FromArray(results);
    }

    [Builtin("pmap")]
    public static object PMap(object fn, params object[] lists)
    {
      if (lists == null)
      {
        return null;
      }

      ICallable f = RequiresNotNull<ICallable>(fn);

      List<object[]> args = new List<object[]>();

      foreach (object[] r in new MultiEnumerable(lists))
      {
        args.Add(r);
      }

      object[] results = new object[args.Count];

      List<Thunk> thunks = new List<Thunk>();

      for (int i = 0; i < results.Length; i++)
      {
        int index = i;
         thunks.Add(
          delegate
          {
            results[index] = f.Call(args[index]);
          });
      }

      PExecute(thunks);

      return Runtime.Cons.FromArray(results);
    }


    [Builtin("map")]
    public static object Map(object fn, object lst)
    {
      Cons list = Requires<Runtime.Cons>(lst);
      ICallable f = RequiresNotNull<ICallable>(fn);
      Cons h = null, head = null;
      while (list != null)
      {
        Cons r = new Cons(f.Call(list.car));
        if (head == null)
        {
          head = h = r;
        }
        else
        {
          h.cdr = r;
          h = r;
        }
        list = list.cdr as Cons;
      }
      return head;
    }

    [Builtin("map")]
    public static object Map(object fn, object list1, object list2)
    {
      return Map(fn, new object[] { list1, list2 });
    }

    [Builtin("map")]
    public static object Map(object fn, params object[] lists)
    {
      // FIXME: this is wrong
      if (lists == null)
      {
        return null;
      }

      if (lists.Length == 0)
      {
        return AssertionViolation("map", "expected at least 1 list", null);
      }

      ICallable f = RequiresNotNull<ICallable>(fn);
      Cons h = null, head = null;

      foreach (object[] obj in new MultiEnumerable(lists))
      {
        Cons r = new Cons(f.Call(obj));
        if (head == null)
        {
          head = h = r;
        }
        else
        {
          h.cdr = r;
          h = r;
        }
      }
      return head;
    }


    [Builtin("for-each")]
    public static object ForEach(object fn, object list)
    {
      Cons c = Requires<Runtime.Cons>(list);
      ICallable f = RequiresNotNull<ICallable>(fn);
      while (c != null)
      {
        f.Call(c.car);
        c = c.cdr as Cons;
      }
      return Unspecified;
    }

    [Builtin("for-each")]
    public static object ForEach(object fn, params object[] lists)
    {
      if (lists != null)
      {
        ICallable f = RequiresNotNull<ICallable>(fn);
        foreach (object[] obj in new MultiEnumerable(lists))
        {
          f.Call(obj);
        }
      }
      return Unspecified;
    }

  }
}
