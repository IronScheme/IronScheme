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
using Microsoft.Scripting.Generation;
using Microsoft.Scripting;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Reflection.Emit;
using System.Collections;

namespace IronScheme.Runtime.R6RS
{
  public abstract class Condition : Exception
  {
    public override string ToString()
    {
      var w = new StringWriter();
      "(display {0} {1})".Eval(this, w);
      return w.GetBuffer();
    }

    public override string Message
    {
      get
      {
        return ToString();
      }
    }
  }

  sealed class CompoundCondition : Condition
  {
    internal object[] conds;
    public CompoundCondition(object[] conds)
    {
      ArrayList inners = new ArrayList();
      foreach (Condition c in conds)
      {
        if (c is CompoundCondition)
        {
          inners.AddRange(((CompoundCondition)c).conds);
        }
        else
        {
          inners.Add(c);
        }
      }
      this.conds = inners.ToArray();
    }

    public override string ToString()
    {
      List<string> c = new List<string>();
      foreach (object e in conds)
      {
        string r = e.ToString();
        if (r.Trim().Length > 0)
        {
          c.Add(r);
        }
      }
      return string.Format("{0}", string.Join(Environment.NewLine, c.ToArray()));
    }
  }

  public class Conditions : Builtins
  {
    [Builtin("condition")]
    public static object Condition(params object[] conds)
    {
      if (conds.Length == 1)
      {
        return conds[0] as Condition;
      }
      return new CompoundCondition(conds);
    }

    [Builtin("simple-conditions")]
    public static object SimpleConditions(object cond)
    {
      if (cond is CompoundCondition)
      {
        return Runtime.Cons.FromArray(((CompoundCondition)cond).conds);
      }
      return List(cond);
    }

    [Builtin("condition?")]
    public static object IsCondition(object cond)
    {
      return GetBool(cond is Exception);
    }

    //(condition-predicate rtd)
    [Builtin("condition-predicate")]
    public static object ConditionPredicate(object rtd)
    {
      RecordTypeDescriptor t = RequiresNotNull<RecordTypeDescriptor>(rtd);

      if (!t.type.IsSubclassOf(typeof(Exception)))
      {
        return AssertionViolation("condition-predicate", "not a valid condition", rtd);
      }

#if CPS
      CallTarget2 p = delegate(object k, object cond)
      {
        CallTarget1 recp = Delegate.CreateDelegate(typeof(CallTarget1), t.predicate) as CallTarget1;

        ICallable kk = k as ICallable;

        if (cond is CompoundCondition)
        {
          CompoundCondition cc = (CompoundCondition)cond;
          foreach (object ic in cc.conds)
          {
            if (IsTrue(recp(ic)))
            {
              return kk.Call(TRUE);
            }
          }
          return kk.Call(FALSE);
        }
        else
        {
          return kk.Call(recp(cond));
        }
      };
#else

      CallTarget1 p = delegate(object cond)
      {
        CallTarget1 recp = Delegate.CreateDelegate(typeof(CallTarget1), t.predicate) as CallTarget1;

        if (cond is CompoundCondition)
        {
          CompoundCondition cc = (CompoundCondition)cond;
          foreach (object ic in cc.conds)
          {
            if (IsTrue(recp(ic)))
            {
              return TRUE;
            }
          }
          return FALSE;
        }
        else
        {
          return recp(cond);
        }
      };
#endif

      return Closure.Make(Context, p);
    }

    //(condition-accessor rtd proc)
    [Builtin("condition-accessor")]
    public static object ConditionAccessor(object rtd, object proc)
    {
      RecordTypeDescriptor t = RequiresNotNull<RecordTypeDescriptor>(rtd);

      if (!t.type.IsSubclassOf(typeof(Exception)))
      {
        return AssertionViolation("condition-accessor", "not a valid condition", rtd);
      }

      ICallable c = RequiresNotNull<ICallable>(proc);

#if CPS
      CallTarget2 p = delegate(object k, object cond)
      {
        ICallable kk = k as ICallable;

        if (cond is CompoundCondition)
        {
          CompoundCondition cc = (CompoundCondition)cond;
          if (cc.conds.Length == 0)
          {
            // error?
            return kk.Call(FALSE);
          }
          else
          {
            foreach (object e in cc.conds)
            {
              if (t.type.IsInstanceOfType(e))
              {
                return OptimizedBuiltins.CallWithK(c, kk, e);
              }
            }
            return kk.Call(FALSE);
          }
        }
        else
        {
          if (t.type.IsInstanceOfType(cond))
          {
            return OptimizedBuiltins.CallWithK(c, kk, cond);
          }
          else
          {
            return kk.Call(FALSE);
          }
        }
      };
#else
      CallTarget1 p = delegate(object cond)
      {
        if (cond is CompoundCondition)
        {
          CompoundCondition cc = (CompoundCondition)cond;
          if (cc.conds.Length == 0)
          {
            // error?
            return FALSE;
          }
          else
          {
            foreach (object e in cc.conds)
            {
              if (t.type.IsInstanceOfType(e))
              {
                return c.Call(e);
              }
            }
            return FALSE;
          }
        }
        else
        {
          if (t.type.IsInstanceOfType(cond))
          {
            return c.Call(cond);
          }
          else
          {
            return FALSE;
          }
        }
      };
#endif
      return Closure.Make(Context, p);
    }

  }
}

