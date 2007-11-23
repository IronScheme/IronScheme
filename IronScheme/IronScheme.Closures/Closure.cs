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
using Microsoft.Scripting;
using Microsoft.Scripting.Actions;

namespace IronScheme.Runtime
{
  public delegate object ConsFromArrayHandler(object[] args);



  public abstract class Closure : ICallable
  {
    readonly string name;

    int paramcount = int.MaxValue;

    public static ConsFromArrayHandler ConsFromArray;

    public override string ToString()
    {
      return name;
    }

    public abstract object Call(object[] args);

    readonly Delegate target;

    Closure(Delegate target, string name, int paramcount)
    {
      if (paramcount > 5 || paramcount < 0)
      {
        paramcount = -1;
      }
      this.paramcount = paramcount;
      this.name = name;
      this.target = target;
    }

    sealed class ContextClosure : Closure
    {
      CodeContext cc;

      public ContextClosure(CodeContext cc, Delegate target, string name, int paramcount)
        : base(target, name, paramcount)
      {
        this.cc = cc;
      }

      public override object Call(object[] args)
      {
        switch (paramcount)
        {
          case -1:
            return ((CallTargetWithContextN)target)(cc, args);
          case 0:
            return ((CallTargetWithContext0)target)(cc);
          case 1:
            return ((CallTargetWithContext1)target)(cc, args[0]);
          case 2:
            return ((CallTargetWithContext2)target)(cc, args[0], args[1]);
          case 3:
            return ((CallTargetWithContext3)target)(cc, args[0], args[1], args[2]);
          case 4:
            return ((CallTargetWithContext4)target)(cc, args[0], args[1], args[2], args[3]);
          case 5:
            return ((CallTargetWithContext5)target)(cc, args[0], args[1], args[2], args[3], args[4]);
          default:
            throw new NotSupportedException();
        }
      }
    }

    sealed class SimpleClosure : Closure
    {
      public SimpleClosure(Delegate target, string name, int paramcount)
        : base(target, name, paramcount)
      {
      }

      public override object Call(object[] args)
      {
        switch (paramcount)
        {
          case -1:
            return ((CallTargetN)target)(args);
          case 0:
            return ((CallTarget0)target)();
          case 1:
            return ((CallTarget1)target)(args[0]);
          case 2:
            return ((CallTarget2)target)(args[0], args[1]);
          case 3:
            return ((CallTarget3)target)(args[0], args[1], args[2]);
          case 4:
            return ((CallTarget4)target)(args[0], args[1], args[2], args[3]);
          case 5:
            return ((CallTarget5)target)(args[0], args[1], args[2], args[3], args[4]);
          default:
            throw new NotSupportedException();
        }
      }
    }

    public static ICallable Make(CodeContext cc, Delegate target, string name)
    {
      string targetname = target.GetType().Name;
      int paramcount = -1;
      paramcount = targetname[targetname.Length - 1] - '0';
      if (targetname.Contains("WithContext"))
      {
        return new ContextClosure(cc, target, name, paramcount);
      }
      else
      {
        return new SimpleClosure(target, name, paramcount);
      }
    }

    public static ICallable MakeVarArgX(CodeContext cc, Delegate target, int paramcount, string name)
    {
      return new VarArgClosure(cc, target, paramcount, name);
    }

    public static ICallable MakeCase(CodeContext cc, string name, Delegate[] targets, int[] arities)
    {
      return new CaseClosure(cc, name, targets, arities);
    }

    sealed class VarArgClosure : Closure
    {
      ICallable realtarget;
      int pcount = 0;

      public VarArgClosure(CodeContext cc, Delegate target, int paramcount, string name)
        : base(target, name, -1)
      {
        pcount = paramcount;
        realtarget = Make(cc, target, name);
      }

      public override object Call(object[] args)
      {
        if (args.Length + 1 < pcount)
        {
          throw RuntimeHelpers.TypeErrorForIncorrectArgumentCount(name, pcount - 1, int.MaxValue, 0, args.Length, false, false);
        }
        object[] newargs = new object[pcount];
        Array.Copy(args, newargs, pcount - 1);
        object[] last = new object[args.Length - pcount + 1];
        Array.Copy(args, pcount - 1, last, 0, last.Length);
        newargs[pcount - 1] = ConsFromArray(last);
        return realtarget.Call(newargs);
      }
    }

    sealed class CaseClosure : Closure
    {
      CodeContext cc;
      Dictionary<int, ICallable> targets = new Dictionary<int, ICallable>();

      public CaseClosure(CodeContext cc, string name, Delegate[] targets, int[] arities)
        : base(null, name, -1)
      {
        this.cc = cc;

        for (int i = 0; i < targets.Length; i++)
        {
          if (arities[i] == -1)
          {
            this.targets.Add(arities[i], MakeVarArgX(cc, targets[i], 1, name));
          }
          else
          {
            this.targets.Add(arities[i], Make(cc, targets[i], name));
          }
        }
      }

      public override object Call(object[] args)
      {
        int arglen = args.Length;

        if (targets.ContainsKey(arglen))
        {
          return targets[arglen].Call(args);
        }
        else if (targets.ContainsKey(-1))
        {
          return targets[-1].Call(args);
        }

        throw RuntimeHelpers.TypeErrorForIncorrectArgumentCount(name, 0, int.MaxValue, 0, args.Length == -1 ? int.MaxValue : arglen, false, false);
      }
    }

  }
}
