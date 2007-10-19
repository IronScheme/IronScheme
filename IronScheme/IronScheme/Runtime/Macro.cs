#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Permissive License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Permissive License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;

namespace IronScheme.Runtime
{
  public abstract class Macro
  {
    readonly string name;
    readonly int paramcount;

    public int ParamCount
    {
      get { return paramcount; }
    } 

    public string Name
    {
      get { return name; }
    } 

    readonly Delegate target;
    Macro(Delegate target, int paramcount, string name)
    {
      this.paramcount = paramcount;
      this.name = name;
      this.target = target;
    }

    public abstract object Call(CodeContext cc, params object[] args);

    sealed class ContextMacro : Macro
    {
      CodeContext cc;

      CallTargetWithContext0 target0 { get { return target as CallTargetWithContext0; } }
      CallTargetWithContext1 target1 { get { return target as CallTargetWithContext1; } }
      CallTargetWithContext2 target2 { get { return target as CallTargetWithContext2; } }
      CallTargetWithContext3 target3 { get { return target as CallTargetWithContext3; } }
      CallTargetWithContext4 target4 { get { return target as CallTargetWithContext4; } }
      CallTargetWithContext5 target5 { get { return target as CallTargetWithContext5; } }

      CallTargetWithContextN targetN { get { return target as CallTargetWithContextN; } }

      public ContextMacro(CodeContext cc, Delegate target, int paramcount, string name) : base(target, paramcount, name)
      {
        this.cc = cc;
      }

      public override object Call(CodeContext context, params object[] args)
      {
        if (targetN != null)
        {
          return targetN(cc, args);
        }
        if (target0 != null)
        {
          return target0(cc);
        }
        if (target1 != null)
        {
          return target1(cc, args[0]);
        }
        if (target2 != null)
        {
          return target2(cc, args[0], args[1]);
        }
        if (target3 != null)
        {
          return target3(cc, args[0], args[1], args[2]);
        }
        if (target4 != null)
        {
          return target4(cc, args[0], args[1], args[2], args[3]);
        }
        if (target5 != null)
        {
          return target5(cc, args[0], args[1], args[2], args[3], args[4]);
        }
        throw new Exception("The method or operation is not implemented.");
      }
    }

    sealed class SimpleMacro : Macro
    {
      CallTarget0 target0 { get { return target as CallTarget0; } }
      CallTarget1 target1 { get { return target as CallTarget1; } }
      CallTarget2 target2 { get { return target as CallTarget2; } }
      CallTarget3 target3 { get { return target as CallTarget3; } }
      CallTarget4 target4 { get { return target as CallTarget4; } }
      CallTarget5 target5 { get { return target as CallTarget5; } }

      CallTargetN targetN { get { return target as CallTargetN; } }

      public SimpleMacro(Delegate target, int paramcount, string name) : base(target, paramcount, name)
      {

      }

      public override object Call(CodeContext context, params object[] args)
      {
        if (targetN != null)
        {
          return targetN(args);
        }
        if (target0 != null)
        {
          return target0();
        }
        if (target1 != null)
        {
          return target1(args[0]);
        }
        if (target2 != null)
        {
          return target2(args[0], args[1]);
        }
        if (target3 != null)
        {
          return target3(args[0], args[1], args[2]);
        }
        if (target4 != null)
        {
          return target4(args[0], args[1], args[2], args[3]);
        }
        if (target5 != null)
        {
          return target5(args[0], args[1], args[2], args[3], args[4]);
        }
        throw new Exception("The method or operation is not implemented.");
      }
    }

    public static Macro Make(CodeContext cc, Delegate target, int paramcount, string name)
    {
      string targetname = target.GetType().Name;
      if (targetname.Contains("WithContext"))
      {
        return new ContextMacro(cc, target, paramcount, name);
      }
      else
      {
        return new SimpleMacro(target, paramcount, name);
      }
    }

    public static Macro MakeVarArg(CodeContext cc, Delegate target, string name)
    {
      return new VarArgMacro(cc, target, target.Method.GetParameters().Length, name);
    }

    public static Macro MakeVarArgX(CodeContext cc, Delegate target, int paramcount, string name)
    {
      return new VarArgMacro(cc, target, paramcount, name);
    }

    sealed class VarArgMacro : Macro
    {
      Macro realtarget;

      public VarArgMacro(CodeContext cc, Delegate target, int paramcount, string name) : base(target, paramcount, name)
      {
        realtarget = Make(cc, target, paramcount, name);
      }

      public override object Invoke(CodeContext context, object arg0)
      {
        object[] args = new object[ParamCount];
        int i = 0;
        Cons c = arg0 as Cons;
        while (c != null)
        {
          args[i++] = c.Car;
          if (i == ParamCount - 1)
          {
            args[i++] = c.Cdr;
            break;
          }
          c = c.Cdr as Cons;
        }


        if (i != ParamCount)
        {
          throw new Exception("bad paramcount");
        }

        return Call(context, args);
      }

      public override object Call(CodeContext context, params object[] args)
      {
        return realtarget.Call(context, args);
      }
    }

    public virtual object Invoke(CodeContext context, object arg0)
    {
      object[] args = new object[ParamCount];
      int i = 0;
      Cons c = arg0 as Cons;
      while (c != null && i < ParamCount)
      {
        args[i++] = c.Car;
        c = c.Cdr as Cons;
      }

      if (i != ParamCount || c != null)
      {
        throw new Exception("bad paramcount");
      }

      return Call(context, args);
    }
  }
}
